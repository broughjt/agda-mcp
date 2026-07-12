{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Load (
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  LoadRequest (..),
  LoadResponse (..),
  load,
  loadTool,
  renderLoadResponse,
) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Data.Aeson (FromJSON (parseJSON), object, withObject, (.:), (.=))
import Data.Map qualified as Map
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
  Interaction' (Cmd_load),
  OutputConstraint_boot (..),
 )
import Agda.Interaction.Output (OutputConstraint)
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  Goals,
  Response,
  Response_boot (..),
  Status (..),
 )
import Agda.Syntax.Abstract (Expr)
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Common.Aspect (TokenBased (..))
import Agda.Syntax.Common.Pretty (render)
import Agda.Syntax.Position qualified
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (
  HighlightingLevel (..),
  HighlightingMethod (..),
  NamedMeta (..),
  TCErr,
  WarningsAndNonFatalErrors (..),
 )
import Agda.TypeChecking.Monad.MetaVars (
  getInteractionRange,
  getMetaRange,
  withInteractionId,
  withMetaId,
 )
import Agda.Utils.FileName (AbsolutePath, absolute)

import AgdaMCP.Position (
  Span,
  fileSpan,
  renderSpan,
  toSpan,
 )
import AgdaMCP.Session (
  ProtocolViolation (ProtocolViolation),
  SessionM,
  fromProtocolResult,
  liftTCM,
  runInteractionM,
 )
import AgdaMCP.Tools.Common (
  AgdaError (AgdaError),
  NonFatalError (..),
  Warning (..),
  failedTail,
  goalName,
  locatedWarnings,
  parseArguments,
  resolveError,
  withSession,
 )

loadTool :: ToolHandler
loadTool =
  toolHandler
    "load"
    ( Just
        "Load and typecheck an Agda source file. Reports open goals, unsolved \
        \hidden metavariables, non-fatal errors, and warnings on success, or \
        \the Agda error if checking fails. Relative paths are resolved against \
        \the server process's working directory; prefer an absolute path when \
        \that directory may be ambiguous."
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
              ]
        )
        (Just ["path"])
    )
    ( either
        (pure . ProcessSuccess . toolTextError)
        ( fmap (ProcessSuccess . toolTextResult . (: []) . renderLoadResponse)
            . withSession
            . load
        )
        . parseArguments
    )

data LoadRequest = LoadRequest FilePath

instance FromJSON LoadRequest where
  parseJSON = withObject "load arguments" $ \o -> LoadRequest <$> o .: "path"

data LoadResponse
  = Loaded [Goal] [HiddenMetavariable] [Warning] [NonFatalError]
  | LoadFailed AgdaError
  | LoadStale
  deriving (Show)

-- A goal (visible interaction metavariable) in a loaded file.
data Goal = Goal
  { goalId :: InteractionId
  , goalSpan :: Span
  , goalShape :: GoalShape
  }
  deriving (Show)

-- Goals and hidden metavariables use only two of `OutputConstraint`'s
-- constructors. The goals response list is built exclusively by `typeOfMetaMI`
-- (BasicOps.hs:889-921), which does cases on `Judgement`'s two
-- constructors. `HasType` becomes `OfType` and `IsSort` becomes `JustSort`. The
-- remaining `OutputConstraint` constructors are used when reifying constraints
-- (`Cmd_constraints`, the `Cmd_goal_type_context*` family of commands), never
-- for goals.
data GoalShape
  = GoalOfType Text
  | GoalSort
  deriving (Show)

-- An unsolved implicit ("hidden") metavariable reported alongside the goals,
-- consisting of a rendered name, a span location (when in the loaded file), and
-- a `GoalShape`.
data HiddenMetavariable = HiddenMetavariable
  { hiddenMetavariableName :: Text
  , hiddenMetavariableSpan :: Maybe Span
  , hiddenMetavariableShape :: GoalShape
  }
  deriving (Show)

load :: LoadRequest -> SessionM LoadResponse
load (LoadRequest path) = do
  responses <-
    runInteractionM $ const $ IOTCM path None Direct (Cmd_load path [])
  parsed <- fromProtocolResult $ parseLoadResponses responses
  resolved <- liftTCM $ resolveLoad path responses parsed
  fromProtocolResult resolved

renderLoadResponse :: LoadResponse -> Text
renderLoadResponse (Loaded goals hiddenMetavariables warnings errors) =
  Text.intercalate "\n\n" $
    concat
      [ [loadedHeader goals hiddenMetavariables warnings errors]
      , case goals of
          [] -> []
          _ -> [Text.intercalate "\n" (map renderGoal goals)]
      , loadSection
          "Unsolved metavariables:"
          (map renderHiddenMetavariable hiddenMetavariables)
      , loadSection "Non-fatal errors:" [e | NonFatalError (_, e) <- errors]
      , loadSection "Warnings:" [w | Warning (_, w) <- warnings]
      ]
renderLoadResponse (LoadFailed (AgdaError message _ warnings)) =
  Text.intercalate "\n\n" $
    ["Load failed:", message]
      <> loadSection "Warnings:" [w | Warning (_, w) <- warnings]
renderLoadResponse LoadStale =
  "The file changed on disk while Agda was checking it, so the result \
  \was discarded. Please load the file again."

loadedHeader ::
  [Goal] -> [HiddenMetavariable] -> [Warning] -> [NonFatalError] -> Text
loadedHeader goals hiddenMetavariables warnings errors =
  verb <> ": " <> Text.intercalate ", " counts <> "."
 where
  verb = case errors of
    [] -> "Load succeeded"
    _ -> "Load completed with " <> counted "non-fatal error" (length errors)
  counts =
    concat
      [ [if null goals then "no goals" else counted "goal" (length goals)]
      , [ counted "unsolved metavariable" (length hiddenMetavariables)
        | not (null hiddenMetavariables)
        ]
      , [counted "warning" (length warnings) | not (null warnings)]
      ]

counted :: Text -> Int -> Text
counted noun n =
  Text.pack (show n) <> " " <> noun <> (if n == 1 then "" else "s")

loadSection :: Text -> [Text] -> [Text]
loadSection _ [] = []
loadSection title items = [title <> "\n\n" <> Text.intercalate "\n" items]

renderGoal :: Goal -> Text
renderGoal (Goal goalId s shape) =
  renderShape (goalName goalId) shape
    <> " (at "
    <> renderSpan s
    <> ")"

renderHiddenMetavariable :: HiddenMetavariable -> Text
renderHiddenMetavariable (HiddenMetavariable name maybeSpan shape) =
  renderShape name shape
    <> maybe "" (\s -> " (at " <> renderSpan s <> ")") maybeSpan

renderShape :: Text -> GoalShape -> Text
renderShape name (GoalOfType ty) = name <> " : " <> ty
renderShape name GoalSort = "Sort " <> name

data LoadResponse'
  = LoadGoals Goals WarningsAndNonFatalErrors [InteractionId]
  | LoadError TCErr
  | LoadNotRegistered

{- The grammar of a Cmd_load response list, following the Agda 2.8.0 source:

exchange := prelude checking
          | failed

prelude := Status ClearRunningInfo ClearHighlighting

checking := RunningInfo checking
          | loaded
          | failed

loaded := Status
          DisplayInfo (Info_AllGoalsWarnings)
          InteractionPoints?

failed := DisplayInfo (Info_Error)
          JumpToError
          HighlightingInfo
          Status
-}
parseLoadResponses ::
  [Response] -> Either (ProtocolViolation Response) LoadResponse'
parseLoadResponses responses = maybe (Left violation) Right (exchange responses)
 where
  violation = ProtocolViolation "Cmd_load" responses

  -- The prelude `Status` is emitted right after `cmd_load'` has cleared
  -- `theCurrentFile`, so it must report the file as not yet checked. Further,
  -- the clear is always for the whole file.
  exchange
    ( Resp_Status status : Resp_ClearRunningInfo
        : Resp_ClearHighlighting NotOnlyTokenBased
        : rest
      )
      | not (sChecked status) = checking rest
  exchange rest = failed rest

  checking (Resp_RunningInfo {} : rest) = checking rest
  checking
    ( Resp_Status status : Resp_DisplayInfo (Info_AllGoalsWarnings goals warnings)
        : rest
      ) =
      loaded status goals warnings rest
  checking rest = failed rest

  loaded _ goals warnings [Resp_InteractionPoints pointIds] = Just (LoadGoals goals warnings pointIds)
  -- No interaction points means the file's mtime changed during checking, so
  -- `cmd_load'` discarded them and left `theCurrentFile` unset. Hence the
  -- `Status` must report the file as unchecked. If it claims checked, our
  -- "stale file" reading of the missing points is wrong.
  loaded status _ _ []
    | not (sChecked status) = Just LoadNotRegistered
  loaded _ _ _ _ = Nothing

  failed = failedTail LoadError

-- Extract everything a `LoadResponse` needs while in the context of the
-- type-checking monad.
resolveLoad ::
  FilePath ->
  [Response] ->
  LoadResponse' ->
  TCM (Either (ProtocolViolation Response) LoadResponse)
resolveLoad path responses response = do
  path' <- liftIO (absolute path)
  case response of
    LoadGoals (visible, hidden) warnings pointIds -> runExceptT $ do
      goals <- traverse toGoal visible

      -- The `goals` and interaction IDs from `Resp_InteractionPoints` read the
      -- same `stIteractionPoints` moments apart, but
      -- `getInteractionIdsAndMetas` drops solved points and points without
      -- metavariables, so we check that our goal ids in `goals` are a subset of
      -- the points in `pointIds` (drawn from `Resp_InteractionPoints`).
      when (any ((`notElem` pointIds) . goalId) goals) $
        throwError violation

      hiddenMetavariables <- traverse (toHiddenMetavariable path') hidden
      lift $
        Loaded goals hiddenMetavariables
          <$> (map Warning <$> locatedWarnings path' (tcWarnings warnings))
          <*> (map NonFatalError <$> locatedWarnings path' (nonFatalErrors warnings))
    LoadError err -> Right . LoadFailed <$> resolveError path' err
    LoadNotRegistered -> pure (Right LoadStale)
 where
  violation = ProtocolViolation "Cmd_load" responses

  toGoal ::
    OutputConstraint Expr InteractionId ->
    ExceptT (ProtocolViolation Response) TCM Goal
  toGoal (OfType pointId ty) =
    -- Render in the interaction point's scope, as the Emacs and JSON
    -- frontends both do (showGoals, BasicOps.hs:830-836; JSONTop.hs:309).
    Goal pointId
      <$> spanOf pointId
      <*> ( GoalOfType . Text.pack . render
              <$> lift (withInteractionId pointId $ prettyATop ty)
          )
  toGoal (JustSort pointId) = flip (Goal pointId) GoalSort <$> spanOf pointId
  -- Unreachable; see the `GoalShape` note.
  toGoal _ = throwError violation

  spanOf :: InteractionId -> ExceptT (ProtocolViolation Response) TCM Span
  spanOf pointId =
    lift (getInteractionRange pointId)
      >>= maybe
        -- Assertion: interaction ids have ranges
        (throwError violation)
        (pure . toSpan)
        . Agda.Syntax.Position.rangeToInterval

  toHiddenMetavariable ::
    AbsolutePath ->
    OutputConstraint Expr NamedMeta ->
    ExceptT (ProtocolViolation Response) TCM HiddenMetavariable
  toHiddenMetavariable file constraint = case constraint of
    OfType metavariable ty ->
      hiddenMetavariable metavariable $
        GoalOfType . Text.pack . render
          <$> lift (withMetaId (nmid metavariable) $ prettyATop ty)
    JustSort metavariable ->
      hiddenMetavariable metavariable (pure GoalSort)
    -- Unreachable; see the `GoalShape` note.
    _ -> throwError violation
   where
    hiddenMetavariable metavariable shape =
      HiddenMetavariable
        <$> renderedName metavariable
        <*> (fileSpan file <$> lift (getMetaRange (nmid metavariable)))
        <*> shape

    renderedName metavariable =
      Text.pack . render
        <$> lift (withMetaId (nmid metavariable) $ prettyATop metavariable)
