{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Goal (
  GoalDetail (..),
  GoalDisplay (..),
  GoalRequest (..),
  GoalResponse (..),
  GoalType (..),
  goal,
  goalTool,
  renderGoalResponse,
) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Data.Aeson (FromJSON (parseJSON), Value, object, withObject, (.:), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  InputSchema (..),
  ProcessResult (..),
  ToolHandler,
  toolHandler,
  toolTextError,
  toolTextResult,
 )

import Agda.Interaction.Base (
  IOTCM' (..),
  Interaction' (
    Cmd_goal_type_context,
    Cmd_goal_type_context_check,
    Cmd_goal_type_context_infer
  ),
  OutputConstraint_boot (..),
  Rewrite (..),
 )
import Agda.Interaction.BasicOps (typeOfMeta)
import Agda.Interaction.Output (OutputForm)
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  GoalDisplayInfo_boot (..),
  GoalTypeAux (..),
  Response,
  ResponseContextEntry,
  Response_boot (..),
 )
import Agda.Syntax.Abstract qualified as A
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common (InteractionId (..))
import Agda.Syntax.Common.Pretty (prettyShow, render)
import Agda.Syntax.Concrete qualified as C
import Agda.Syntax.Position (noRange)
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (
  Closure (clValue),
  HighlightingLevel (..),
  HighlightingMethod (..),
  IPFace',
  InteractionError (NoSuchInteractionPoint),
  TCErr (IOException, TypeError),
  TypeError (InteractionError),
 )
import Agda.TypeChecking.Monad.MetaVars (withInteractionId)
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.Utils.FileName (absolute)

import AgdaMCP.Session (
  ProtocolViolation (ProtocolViolation),
  SessionM,
  fromProtocolResult,
  liftTCM,
  runInteractionM,
 )
import AgdaMCP.Tools.Common (
  AgdaError,
  agdaErrorSpan,
  failedTail,
  goalName,
  parseArguments,
  renderAgdaError,
  resolveError,
  targetIsLoaded,
  withSession,
 )
import AgdaMCP.Tools.Load (
  ContextEntry,
  GoalShape (..),
  LoadRequest (..),
  LoadResponse,
  load,
  renderContext,
  renderLoadResponse,
  renderShape,
  resolveContext,
 )

goalTool :: ToolHandler
goalTool =
  toolHandler
    "goal"
    ( Just
        "Inspect a single open goal in the currently loaded Agda file, without \
        \modifying anything. With just a goal ID, reports the goal's type, its \
        \local context, and any unsolved constraints mentioning the goal. The \
        \type is reported as stated plus fully normalized when the two differ, \
        \or at the requested `normalization` only. With an `expression`, \
        \instead reports the goal's type, the expression's inferred type \
        \(Have), and the expression's elaboration checked against the goal \
        \(Checks). Infer and check are independent, so one can succeed while \
        \the other fails. Goal interaction IDs are only valid for the most \
        \recently loaded file. A query against any other file is refused and \
        \returns that file's fresh load result to query against instead. \
        \Relative paths are resolved against the server process's working \
        \directory. Prefer an absolute path when that directory may be \
        \ambiguous."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [
                ( "path"
                , object
                    [ "type" .= ("string" :: Text)
                    , "description"
                        .= ( "Path to an Agda source file (.agda, but also \
                             \literate formats such as .lagda.md, .lagda.tex, \
                             \.lagda.typ, etc). Relative paths are resolved \
                             \against the server process's working directory." ::
                               Text
                           )
                    ]
                )
              ,
                ( "goal"
                , object
                    [ "type" .= ("integer" :: Text)
                    , "description"
                        .= ( "The target goal's interaction ID (`?N`) from a \
                             \load result" ::
                               Text
                           )
                    ]
                )
              ,
                ( "normalization"
                , object
                    [ "type" .= ("string" :: Text)
                    , "enum" .= (Map.keys normalizations :: [Text])
                    , "description"
                        .= ( "How much to normalize the reported types. When \
                             \omitted, the goal type is reported as stated \
                             \plus fully normalized when that differs." ::
                               Text
                           )
                    ]
                )
              ,
                ( "expression"
                , object
                    [ "type" .= ("string" :: Text)
                    , "description"
                        .= ( "An Agda expression to infer the type of and to \
                             \check against the goal" ::
                               Text
                           )
                    ]
                )
              ]
        )
        (Just ["path", "goal"])
    )
    ( either
        (pure . ProcessSuccess . toolTextError)
        ( fmap (ProcessSuccess . toolTextResult . (: []) . renderGoalResponse)
            . withSession
            . goal
        )
        . parseArguments
    )

data GoalRequest
  = GoalRequest FilePath InteractionId (Maybe Rewrite) (Maybe String)

instance FromJSON GoalRequest where
  parseJSON = withObject "goal arguments" $ \o ->
    GoalRequest
      <$> o .: "path"
      <*> (InteractionId <$> o .: "goal")
      <*> Aeson.explicitParseFieldMaybe parseNormalization o "normalization"
      <*> Aeson.explicitParseFieldMaybe parseExpression o "expression"

normalizations :: Map.Map Text Rewrite
normalizations =
  Map.fromList
    [ ("asis", AsIs)
    , ("instantiated", Instantiated)
    , ("headnormal", HeadNormal)
    , ("simplified", Simplified)
    , ("normalized", Normalised)
    ]

parseNormalization :: Value -> Aeson.Parser Rewrite
parseNormalization = Aeson.withText "normalization" $ \name ->
  case Map.lookup name normalizations of
    Just rewrite -> pure rewrite
    Nothing ->
      fail $
        "expected one of "
          <> Text.unpack (Text.intercalate ", " (Map.keys normalizations))

parseExpression :: Value -> Aeson.Parser String
parseExpression value = do
  expression <- Text.strip <$> parseJSON value
  when (Text.null expression) $
    fail "'expression' is empty or whitespace-only"
  pure $ Text.unpack expression

data GoalResponse
  = -- The queried goal, with either its plain display or the expression
    -- results.
    GoalDisplayed GoalDisplay
  | -- The query named an interaction ID that doesn't exist in the loaded file
    -- (Agda's `NoSuchInteractionPoint`).
    GoalUnknown InteractionId
  | -- The query failed for a reason other than a bad interaction ID (only
    -- environmental causes are known to reach this).
    GoalFailed AgdaError
  | -- The target file is not the currently loaded file. Goal interaction IDs
    -- are only valid for the most recently loaded file, so nothing was
    -- queried; the load supplies the IDs the caller was missing.
    GoalNotLoaded LoadResponse
  deriving (Eq, Show)

data GoalDisplay = GoalDisplay
  { displayGoal :: InteractionId
  , displayType :: GoalType
  , displayDetail :: GoalDetail
  }
  deriving (Eq, Show)

-- The goal's type at the requested normalization (or as stated when none was
-- requested), plus the fully normalized shape when no normalization was
-- requested. Whether the normalized rendering is shown is a presentation
-- decision (`renderGoalResponse` shows it only when it differs textually).
data GoalType = GoalType
  { goalTypeStated :: GoalShape
  , goalTypeNormalized :: Maybe GoalShape
  }
  deriving (Eq, Show)

data GoalDetail
  = -- A query for a goal's local context (at the requested normalization), its
    -- cubical boundary, and the unsolved constraints that mention it.
    PlainGoal
      { plainContext :: [ContextEntry]
      , plainBoundary :: [Text]
      , plainConstraints :: [Text]
      }
  | -- An expression query, consisting of the submitted expression, its inferred
    -- type, and its elaboration checked against the goal. Inference and
    -- checking are independent. For example, type mismatches can infer but not
    -- check, while some expressions infer only a fresh-meta type.
    ExpressionGoal
      { expressionSubmitted :: Text
      , expressionHave :: Either AgdaError Text
      , expressionChecks :: Either AgdaError Text
      }
  deriving (Eq, Show)

goal :: GoalRequest -> SessionM GoalResponse
goal (GoalRequest path goalId normalization maybeExpression) = do
  -- Goal interaction IDs are only meaningful against a load result the caller
  -- has seen (see `targetIsLoaded`). Without this check Agda would load the
  -- file implicitly and interpret the ID against fresh interaction points the
  -- caller never saw.
  loaded <- targetIsLoaded path
  if not loaded
    then GoalNotLoaded <$> load (LoadRequest path)
    else maybe plainGoal expressionGoal maybeExpression
 where
  normalization' = fromMaybe AsIs normalization

  command interaction = const $ IOTCM path None Direct interaction

  plainGoal = do
    responses <-
      runInteractionM $
        command $
          Cmd_goal_type_context normalization' goalId noRange ""
    parsed <-
      fromProtocolResult $
        parseGoalTypeResponses
          "Cmd_goal_type_context"
          goalId
          normalization'
          plainAux
          responses
    resolved <-
      liftTCM $ resolvePlainGoal path goalId normalization responses parsed
    fromProtocolResult resolved

  -- Both interactions run unconditionally, since inference and checking are
  -- independent. A failed command doesn't change the session state, so a failed
  -- infer doesn't break or skip the check.
  expressionGoal expression = do
    inferResponses <-
      runInteractionM $
        command $
          Cmd_goal_type_context_infer normalization' goalId noRange expression
    inferParsed <-
      fromProtocolResult $
        parseGoalTypeResponses
          "Cmd_goal_type_context_infer"
          goalId
          normalization'
          inferAux
          inferResponses
    checkResponses <-
      runInteractionM $
        command $
          Cmd_goal_type_context_check normalization' goalId noRange expression
    checkParsed <-
      fromProtocolResult $
        parseGoalTypeResponses
          "Cmd_goal_type_context_check"
          goalId
          normalization'
          checkAux
          checkResponses
    resolved <-
      liftTCM $
        resolveExpressionGoal
          path
          goalId
          normalization
          (Text.pack expression)
          (inferResponses <> checkResponses)
          (fst <$> inferParsed)
          (fst <$> checkParsed)
    fromProtocolResult resolved

  -- A plain query's payload carries no auxiliary information
  -- (`interpret Cmd_goal_type_context`, InteractionTop.hs:724-725).
  plainAux GoalOnly = Just ()
  plainAux _ = Nothing

  -- The infer payload carries the expression's type. Its "actual" boundary
  -- faces are not rendered (`interpret Cmd_goal_type_context_infer`,
  -- InteractionTop.hs:727-738; the all-whitespace fallback to GoalOnly there is
  -- unreachable because blank expressions are rejected at argument parsing).
  inferAux (GoalAndHave ty _) = Just ty
  inferAux _ = Nothing

  -- The check payload carries the reified elaboration of the expression
  -- against the goal type (`interpret Cmd_goal_type_context_check`,
  -- InteractionTop.hs:740-748).
  checkAux (GoalAndElaboration term) = Just term
  checkAux _ = Nothing

-- The payload of a successful goal-type exchange: the command-specific
-- auxiliary information and the goal's context. The boundary and constraints
-- are dropped here for the infer and check exchanges (`fst <$>` above), whose
-- rendering doesn't repeat them.
type GoalTypePayload aux =
  (aux, ([ResponseContextEntry], [IPFace' C.Expr], [OutputForm C.Expr C.Expr]))

{- The grammar of a Cmd_goal_type_context{,_infer,_check} response list,
following the Agda 2.8.0 source:

exchange := Status                        -- display_info emits Resp_Status
                                          -- first (InteractionTop.hs:1143-1146
                                          -- via displayStatus :1135-1137); its
                                          -- sChecked is deliberately not
                                          -- asserted: a module with open goals
                                          -- reports sChecked = False (observed
                                          -- against 2.8.0), and `status` also
                                          -- compares the loaded file's on-disk
                                          -- mtime, which a disk edit between
                                          -- load and goal legitimately flips
            DisplayInfo (Info_GoalSpecific ii (Goal_GoalType norm aux ctx boundary constraints))
                                          -- cmd_goal_type_context_and
                                          -- (:1061-1067); ii and norm must
                                          -- match what we sent, and aux must
                                          -- match the command's shape
          | failed                        -- handleErr (:216-242), the shared
                                          -- failedTail; an error can strike in
                                          -- the interaction-point lookup
                                          -- (NoSuchInteractionPoint) or, for
                                          -- infer/check, while parsing or
                                          -- checking the expression
                                          -- (:727-748), all before
                                          -- display_info runs

There is no InteractionPoints token: `updateInteractionPointsAfter` is False
for the whole Cmd_goal_type_context family (:456-458). The file was already
verified loaded (`targetIsLoaded`), so an implicit-load prelude here is a
protocol violation.
-}
parseGoalTypeResponses ::
  String ->
  InteractionId ->
  Rewrite ->
  (GoalTypeAux -> Maybe aux) ->
  [Response] ->
  Either
    (ProtocolViolation Response)
    (Either TCErr (GoalTypePayload aux))
parseGoalTypeResponses command goalId norm matchAux responses =
  maybe (Left violation) Right (exchange responses)
 where
  violation = ProtocolViolation command responses

  exchange
    [ Resp_Status _
      , Resp_DisplayInfo
          (Info_GoalSpecific goalId' (Goal_GoalType norm' aux ctx boundary constraints))
      ]
      | goalId' == goalId
      , norm' == norm
      , Just aux' <- matchAux aux =
          Just (Right (aux', (ctx, boundary, constraints)))
  exchange rest = failedTail Left rest

resolvePlainGoal ::
  FilePath ->
  InteractionId ->
  Maybe Rewrite ->
  [Response] ->
  Either TCErr (GoalTypePayload ()) ->
  TCM (Either (ProtocolViolation Response) GoalResponse)
resolvePlainGoal path goalId normalization responses parsed = case parsed of
  Left e -> resolveFailure violation path goalId e
  Right ((), (ctx, boundary, constraints)) -> runExceptT $ do
    goalType <- resolveGoalType violation goalId normalization
    context <- lift $ resolveContext goalId ctx
    -- The boundary and the constraints render exactly as Agda's own frontends
    -- render them: `pretty` for boundary faces (JSONTop.hs:396) and
    -- `prettyTCM` in the goal's scope for constraints (EmacsTop.hs:241-246).
    constraintTexts <-
      lift $
        withInteractionId goalId $
          traverse (fmap (Text.pack . render) . prettyTCM) constraints
    pure $
      GoalDisplayed $
        GoalDisplay goalId goalType $
          PlainGoal context (map (Text.pack . prettyShow) boundary) constraintTexts
 where
  violation = ProtocolViolation "Cmd_goal_type_context" responses

resolveExpressionGoal ::
  FilePath ->
  InteractionId ->
  Maybe Rewrite ->
  Text ->
  [Response] ->
  Either TCErr A.Expr ->
  Either TCErr A.Expr ->
  TCM (Either (ProtocolViolation Response) GoalResponse)
resolveExpressionGoal path goalId normalization submitted responses inferResult checkResult =
  case (failedLookup inferResult, failedLookup checkResult) of
    -- A bogus interaction ID fails the lookup in both commands. Require both
    -- errors to name the ID we sent; a mixed or mismatched pair contradicts
    -- the model and is therefore a protocol violation.
    (Just inferGoal, Just checkGoal)
      | inferGoal == goalId && checkGoal == goalId ->
          pure $ Right $ GoalUnknown goalId
      | otherwise -> pure $ Left violation
    (Just _, Nothing) -> pure $ Left violation
    (Nothing, Just _) -> pure $ Left violation
    (Nothing, Nothing) -> case firstIOFailure of
      -- An IOException is environmental rather than a failure to infer or
      -- check the submitted expression. Treat the whole query as failed so
      -- the renderer does not claim its location is expression-relative.
      Just e -> Right <$> resolveGoalError path e
      Nothing -> runExceptT $ do
        goalType <- resolveGoalType violation goalId normalization
        have <- lift $ resolveResult inferResult
        checks <- lift $ resolveResult checkResult
        pure $
          GoalDisplayed $
            GoalDisplay goalId goalType (ExpressionGoal submitted have checks)
 where
  violation =
    ProtocolViolation "Cmd_goal_type_context_infer/check" responses

  failedLookup = either noSuchInteractionPoint (const Nothing)

  firstIOFailure = case (inferResult, checkResult) of
    (Left e@(IOException _ _ _), _) -> Just e
    (_, Left e@(IOException _ _ _)) -> Just e
    _ -> Nothing

  resolveResult (Left e) = do
    path' <- liftIO (absolute path)
    Left <$> resolveError path' e
  resolveResult (Right expr) =
    Right . Text.pack . render <$> withInteractionId goalId (prettyATop expr)

-- Distinguish a failure caused by the queried goal not existing from other
-- failures of the goal-type commands. A bogus interaction ID fails
-- `withInteractionId`'s `lookupInteractionPoint` (MetaVars.hs:638-640) with
-- this dedicated constructor (Base.hs:5412/5438).
noSuchInteractionPoint :: TCErr -> Maybe InteractionId
noSuchInteractionPoint e
  | TypeError _ _ closure <- e
  , InteractionError (NoSuchInteractionPoint goalId) <- clValue closure =
      Just goalId
  | otherwise = Nothing

resolveFailure ::
  ProtocolViolation Response ->
  FilePath ->
  InteractionId ->
  TCErr ->
  TCM (Either (ProtocolViolation Response) GoalResponse)
resolveFailure violation path goalId e = case noSuchInteractionPoint e of
  Just failedGoal
    | failedGoal == goalId -> pure $ Right $ GoalUnknown goalId
    | otherwise -> pure $ Left violation
  Nothing -> Right <$> resolveGoalError path e

resolveGoalError :: FilePath -> TCErr -> TCM GoalResponse
resolveGoalError path e = do
  path' <- liftIO $ absolute path
  GoalFailed <$> resolveError path' e

-- The goal's type isn't part of the `Goal_GoalType` payload, so we query it
-- with `typeOfMeta` and render in the interaction point's scope, as JSONTop
-- does (`prettyTypeOfMeta`, JSONTop.hs:392-398). `typeOfMeta` reports goals
-- through the same two `Judgement`-derived shapes as the load goals list
-- (`typeOfMetaMI`, BasicOps.hs:889-921), so the same `GoalShape` note in
-- `AgdaMCP.Tools.Load` applies and other constructors are violations. The
-- judgement is independent of the normalization, so the normalized shape (for
-- requests without a normalization) can only differ in the type's rendering.
resolveGoalType ::
  ProtocolViolation Response ->
  InteractionId ->
  Maybe Rewrite ->
  ExceptT (ProtocolViolation Response) TCM GoalType
resolveGoalType violation goalId maybeNormalization =
  GoalType
    <$> shapeAt (fromMaybe AsIs maybeNormalization)
    <*> traverse shapeAt (maybe (Just Normalised) (const Nothing) maybeNormalization)
 where
  shapeAt normalization = do
    form <- lift $ withInteractionId goalId $ typeOfMeta normalization goalId
    case form of
      OfType _ ty ->
        GoalOfType . Text.pack . render
          <$> lift (withInteractionId goalId $ prettyATop ty)
      JustSort _ -> pure GoalSort
      _ -> throwError violation

renderGoalResponse :: GoalResponse -> Text
renderGoalResponse (GoalNotLoaded reload) =
  "Goal query refused: the file is not the currently loaded file, and goal \
  \interaction IDs are only valid for the most recently loaded file. Loaded \
  \the file; use the goal IDs from the fresh result below:\n\n"
    <> renderLoadResponse reload
renderGoalResponse (GoalUnknown goalId) =
  "No such goal "
    <> goalName goalId
    <> " in the loaded file. Goal IDs renumber after every edit or reload; \
       \use the IDs from the most recent load result."
renderGoalResponse (GoalFailed e) =
  "The goal query failed:\n\n" <> Text.intercalate "\n" (renderAgdaError e)
renderGoalResponse (GoalDisplayed (GoalDisplay goalId goalType detail)) =
  case detail of
    PlainGoal context boundary constraints ->
      Text.intercalate "\n\n" $
        concat
          [ [Text.intercalate "\n" (goalLines <> renderContext context)]
          , goalSection "Boundary (wanted):" boundary
          , goalSection "Constraints on this goal:" constraints
          ]
    ExpressionGoal submitted have checks ->
      Text.intercalate "\n\n" $
        [ Text.intercalate "\n" goalLines
        , either
            (renderExpressionFailure "Infer failed")
            (\ty -> "Have: " <> submitted <> " : " <> ty)
            have
        , either
            (renderExpressionFailure "Check failed")
            ("Checks: elaborates to " <>)
            checks
        ]
 where
  goalLines =
    renderShape (goalName goalId) (goalTypeStated goalType)
      : normalizedLine

  -- The normalized rendering is shown only when it differs textually from
  -- the stated one. The shapes can't disagree (see `resolveGoalType`), so
  -- sort goals never show the line.
  normalizedLine =
    case (goalTypeStated goalType, goalTypeNormalized goalType) of
      (GoalOfType stated, Just (GoalOfType normalized))
        | normalized /= stated -> ["normalized: " <> normalized]
      _ -> []

goalSection :: Text -> [Text] -> [Text]
goalSection _ [] = []
goalSection title items = [title <> "\n\n" <> Text.intercalate "\n" items]

-- Expression parse and type errors carry positions relative to the submitted
-- expression, not the file (the noRange artifact; see the give design notes
-- in the project plan), hence the parenthetical. When the error has a file
-- span (only environmental failures are known to), the message already
-- carries Agda's own location text.
renderExpressionFailure :: Text -> AgdaError -> Text
renderExpressionFailure label e =
  label
    <> note
    <> ":\n\n"
    <> Text.intercalate "\n" (renderAgdaError e)
 where
  note = case agdaErrorSpan e of
    Nothing -> " (locations are relative to the submitted expression)"
    Just _ -> ""
