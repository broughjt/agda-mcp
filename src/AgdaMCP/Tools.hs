{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools (loadTool, giveTool) where

import Control.Applicative (liftA3, (<|>))
import Control.Exception (
  Exception,
  Handler (..),
  IOException,
  catches,
  displayException,
  throwIO,
 )
import Control.Monad (guard, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (toList)
import Data.List (find, nub, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LazyText
import System.AtomicWrite.Writer.ByteString (atomicWriteFile)

import Agda.Interaction.Base (
  IOTCM' (..),
  Interaction' (Cmd_give, Cmd_load),
  OutputConstraint_boot (..),
  UseForce (WithoutForce),
 )
import Agda.Interaction.EmacsTop (showInfoError)
import Agda.Interaction.Output (OutputConstraint)
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  GiveResult (..),
  Goals,
  Info_Error,
  Info_Error_boot (..),
  RemoveTokenBasedHighlighting (..),
  Response,
  Response_boot (..),
  Status (..),
 )
import Agda.Syntax.Abstract (Expr)
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common (InteractionId (..))
import Agda.Syntax.Common.Aspect (TokenBased (..))
import Agda.Syntax.Common.Pretty (render)
import Agda.Syntax.Position (getRange, noRange)
import Agda.Syntax.Position qualified
import Agda.TypeChecking.Errors (getAllWarningsOfTCErr, renderError)
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (
  HighlightingLevel (..),
  HighlightingMethod (..),
  NamedMeta (..),
  TCWarning (tcWarningRange),
  WarningsAndNonFatalErrors (..),
 )
import Agda.TypeChecking.Monad.MetaVars (
  getInteractionRange,
  getMetaRange,
  withInteractionId,
  withMetaId,
 )
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.TypeChecking.Pretty.Warning (filterTCWarnings)
import Agda.Utils.FileName (AbsolutePath, absolute)
import Agda.Utils.IO.UTF8 (ReadException, readTextFile)
import Agda.Utils.Maybe.Strict qualified as Strict
import MCP.Server (
  InputSchema (..),
  ProcessResult (..),
  ToolHandler,
  toolHandler,
  toolTextError,
  toolTextResult,
 )

import AgdaMCP.Worker (
  Command (..),
  ProtocolViolation (ProtocolViolation),
  Worker,
  sendCommand,
 )

-- Load

loadTool :: Worker -> ToolHandler
loadTool worker =
  toolHandler
    "agda_load"
    ( Just
        -- TODO:
        "Load and typecheck an Agda file. Reports the open goals \
        \(interaction points) on success, or the error if the file fails \
        \to typecheck. Prefer absolute paths; relative paths are resolved \
        \against the server's working directory."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [
                ( "path"
                , object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("Path to the .agda file to load" :: Text)
                    ]
                )
              ]
        )
        (Just ["path"])
    )
    ( either
        (pure . ProcessSuccess . toolTextError)
        ( liftIO
            . fmap (ProcessSuccess . toolTextResult . (: []) . renderLoadResponse)
            . load worker
        )
        . parseLoadArguments
    )

data LoadRequest = LoadRequest FilePath

data LoadResponse
  = Loaded [Goal] [HiddenMetavariable] [Warning] [NonFatalError]
  | LoadFailed Text (Maybe Span) [Warning]
  | LoadStale
  deriving (Show)

-- A goal (visible interaction metavariable) of the loaded file.
data Goal = Goal
  { goalId :: InteractionId
  , goalSpan :: Span
  , goalShape :: GoalShape
  }
  deriving (Show)

-- There are only two shapes goals and hidden metavariables can take: either
-- `OfType` for a typing judgment or `JustSort` for `IsSort`. The other
-- constructors of `OutputConstraint` are produced for `Cmd_constraints`, never
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

newtype Warning = Warning (Maybe Span, Text)
  deriving (Show)

newtype NonFatalError = NonFatalError (Maybe Span, Text)
  deriving (Show)

load :: Worker -> LoadRequest -> IO LoadResponse
load worker request = runCommand worker (loadCommand request)

parseLoadArguments :: Maybe (Map Text Value) -> Either Text LoadRequest
parseLoadArguments arguments =
  case Map.lookup "path" (fromMaybe Map.empty arguments) of
    Just (Aeson.String path) -> Right (LoadRequest (Text.unpack path))
    _ -> Left "Missing or invalid 'path' argument: expected a string"

loadCommand :: LoadRequest -> Command LoadResponse
loadCommand (LoadRequest path) =
  Command
    { commandIOTCM = IOTCM path None Direct (Cmd_load path [])
    , commandParse = \responses ->
        either
          (pure . Left)
          (resolveLoad path responses)
          (parseLoadResponses responses)
    }

renderLoadResponse :: LoadResponse -> Text
renderLoadResponse (Loaded goals hiddenMetavariables warnings errors) =
  Text.intercalate "\n" $
    concat
      [ ["Load succeeded. Open goals: " <> Text.pack (show (length goals)) <> "."]
      , map renderGoal goals
      , section
          "Unsolved hidden metas:"
          (map renderHiddenMetavariable hiddenMetavariables)
      , section "Non-fatal errors:" [e | NonFatalError (_, e) <- errors]
      , section "Warnings:" [w | Warning (_, w) <- warnings]
      ]
renderLoadResponse (LoadFailed message _ warnings) =
  Text.intercalate "\n" $
    concat
      [ ["Load failed:", message]
      , section "Warnings:" [w | Warning (_, w) <- warnings]
      ]
renderLoadResponse LoadStale =
  "The file changed on disk while Agda was checking it, so the result \
  \was discarded. Please load the file again."

-- A titled block of lines, omitted entirely when there are no items.
section :: Text -> [Text] -> [Text]
section _ [] = []
section title items = "" : title : items

renderGoal :: Goal -> Text
renderGoal (Goal ii sp shape) =
  renderShape ("?" <> Text.pack (show (interactionId ii))) shape
    <> "  (at "
    <> renderSpan sp
    <> ")"

renderHiddenMetavariable :: HiddenMetavariable -> Text
renderHiddenMetavariable (HiddenMetavariable name sp shape) =
  renderShape name shape <> maybe "" (\s -> "  (at " <> renderSpan s <> ")") sp

renderShape :: Text -> GoalShape -> Text
renderShape name (GoalOfType ty) = name <> " : " <> ty
renderShape name GoalSort = "Sort " <> name

data LoadResponse'
  = LoadGoals Goals WarningsAndNonFatalErrors [InteractionId]
  | LoadError Info_Error
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

  loaded _ goals warnings [Resp_InteractionPoints ids] = Just (LoadGoals goals warnings ids)
  -- No interaction points means the file's mtime changed during checking, so
  -- `cmd_load'` discarded them and left `theCurrentFile` unset. Hence the
  -- `Status` must report the file as unchecked. If it claims checked, our
  -- "stale file" reading of the missing points is wrong.
  loaded status _ _ []
    | not (sChecked status) = Just LoadNotRegistered
  loaded _ _ _ _ = Nothing

  failed = failedTail LoadError

-- Extract everything the pure `LoadResponse` needs from the type-checking
-- monad: rendered goal types (in each goal's scope), interaction ranges, and
-- rendered warnings. This runs on the worker right after the command
-- executed, when the TCState still matches the exchange.

-- Using the TCM, convert a `LoadResponse'` to a `LoadResponse`. We collect the rendered goal types
-- TODO:
resolveLoad ::
  FilePath ->
  [Response] ->
  LoadResponse' ->
  TCM (Either (ProtocolViolation Response) LoadResponse)
resolveLoad path responses response = do
  path' <- liftIO (absolute path)
  case response of
    LoadGoals (visible, hidden) warnings ids -> runExceptT $ do
      goals <- traverse toGoal visible

      -- The `goals` and interaction ids from `Resp_InteractionPoints` read the
      -- same `stIteractionPoints` moments apart, but
      -- `getInteractionIdsAndMetas` drops solved points and points without
      -- metavariables, so we check that our goal ids in `goals` are a subset of
      -- the points in `ids` (drawn from `Resp_InteractionPoints`).
      when (any ((`notElem` ids) . goalId) goals) $
        throwError violation

      hiddenMetavariables <- traverse (toHiddenMetavariable path') hidden
      lift $
        Loaded goals hiddenMetavariables
          <$> (map Warning <$> locatedWarnings path' (tcWarnings warnings))
          <*> (map NonFatalError <$> locatedWarnings path' (nonFatalErrors warnings))
    LoadError (Info_GenericError err) ->
      Right
        <$> liftA3
          LoadFailed
          (Text.pack <$> renderError err)
          (pure (fileSpan path' (getRange err)))
          (map Warning <$> (getAllWarningsOfTCErr err >>= locatedWarnings path'))
    -- TODO: Should this check be shared by `failedTail`?
    --
    -- The other `Info_Error` constructors cannot come from `Cmd_load`:
    -- `Info_CompilationError` only from Cmd_compile (InteractionTop.hs:491),
    -- `Info_Highlighting{Parse,ScopeCheck}Error` only from Cmd_highlight
    -- (:631-633).
    LoadError _ -> pure (Left violation)
    LoadNotRegistered -> pure (Right LoadStale)
 where
  violation = ProtocolViolation "Cmd_load" responses

  toGoal ::
    OutputConstraint Expr InteractionId ->
    ExceptT (ProtocolViolation Response) TCM Goal
  toGoal (OfType i ty) =
    -- Render in the interaction point's scope, as the Emacs and JSON
    -- frontends both do (showGoals, BasicOps.hs:830-836; JSONTop.hs:309).
    Goal i
      <$> spanOf i
      <*> ( GoalOfType . Text.pack . render
              <$> lift (withInteractionId i $ prettyATop ty)
          )
  toGoal (JustSort i) = flip (Goal i) GoalSort <$> spanOf i
  -- Unreachable; see the `GoalShape` note.
  toGoal _ = throwError violation

  spanOf :: InteractionId -> ExceptT (ProtocolViolation Response) TCM Span
  spanOf i =
    lift (getInteractionRange i)
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
    -- Unreachable per the `GoalShape` note.
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

  -- Apply Agda's own warning filtering (removes unsolved-constraint warnings in
  -- the case that there are no "interesting" constraints), then pair each
  -- rendered warning with its span.
  locatedWarnings :: AbsolutePath -> Set TCWarning -> TCM [(Maybe Span, Text)]
  locatedWarnings path' warnings =
    filterTCWarnings warnings >>= traverse locate
   where
    locate warning =
      ((,) (fileSpan path' (tcWarningRange warning)) . Text.pack . render)
        <$> prettyTCM warning

-- Give

giveTool :: Worker -> ToolHandler
giveTool worker =
  toolHandler
    "agda_give"
    ( Just
        -- TODO:
        "Fill one or more goals of a loaded Agda file with expressions. Takes \
        \the file `path` and a list of `gives`, each a `goal` (interaction id, \
        \as reported by agda_load) and an `expression`. All gives are checked \
        \against the currently loaded state, then applied to the file together \
        \and the file is reloaded. If any give fails to typecheck the whole \
        \call is a no-op (the file is left unchanged). After a successful call \
        \the interaction ids are renumbered by the reload, so read the new \
        \goals from the result before giving again."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [
                ( "path"
                , object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("Path to a .agda file" :: Text)
                    ]
                )
              ,
                ( "gives"
                , object
                    [ "type" .= ("array" :: Text)
                    , "description"
                        .= ("The goals to fill and the expressions to fill them with" :: Text)
                    , "items"
                        .= object
                          [ "type" .= ("object" :: Text)
                          , "properties"
                              .= object
                                [ "goal"
                                    .= object
                                      [ "type" .= ("integer" :: Text)
                                      , "description" .= ("The interaction id of the goal" :: Text)
                                      ]
                                , "expression"
                                    .= object
                                      [ "type" .= ("string" :: Text)
                                      , "description" .= ("The expression to give" :: Text)
                                      ]
                                ]
                          , "required" .= (["goal", "expression"] :: [Text])
                          ]
                    ]
                )
              ]
        )
        (Just ["path", "gives"])
    )
    ( either
        (pure . ProcessSuccess . toolTextError)
        ( liftIO
            . fmap (ProcessSuccess . toolTextResult . (: []) . renderGiveResponse)
            . give worker
        )
        . parseGiveArguments
    )

data GiveRequest = GiveRequest FilePath [GiveItem]

data GiveResponse
  = -- Every give typechecked and its edit was spliced into the file.
    GiveApplied [Edit] LoadResponse
  | -- A give failed to typecheck, so the file was left untouched. Carries the
    -- goal that failed, its rendered error, and how many earlier gives in the
    -- same call were discarded.
    GiveRejected InteractionId Text Int LoadResponse
  | -- A goal's recorded span no longer holds a hole (the file changed on disk
    -- since it was loaded), so the edit was refused and nothing was written.
    GiveStale Edit LoadResponse
  | -- Reading or writing the file failed (deleted, permissions, disk full, ...).
    -- Nothing was written -- the write is atomic, so a failed write leaves the
    -- original intact. Carries the rendered I/O error.
    GiveIOError Text LoadResponse
  deriving (Show)

-- TODO: Merge with or pull out of GiveRejected
data RejectedGive = RejectedGive InteractionId Text Int

type GiveItem = (InteractionId, String)

-- TODO: Does Aeson have macros or derives that do this for us?
parseGiveArguments :: Maybe (Map Text Value) -> Either Text GiveRequest
parseGiveArguments arguments = do
  let arguments' = fromMaybe Map.empty arguments
  path <- case Map.lookup "path" arguments' of
    Just (Aeson.String p) -> Right (Text.unpack p)
    _ -> Left "Missing or invalid 'path' argument: expected a string"
  givesValue <-
    maybe
      (Left "Missing 'gives' argument: expected an array")
      Right
      (Map.lookup "gives" arguments')
  items <- parseGiveItems givesValue
  when (null items) $
    Left "The 'gives' array must contain at least one {goal, expression} object"
  let goals = [g | (g, _) <- items]
  when (length (nub goals) /= length goals) $
    Left "Duplicate goal ids in 'gives'; each goal may be given only once per call"
  pure (GiveRequest path items)
 where
  parseGiveItems :: Value -> Either Text [GiveItem]
  parseGiveItems (Aeson.Array items) = traverse parseGiveItem (toList items)
  parseGiveItems _ = Left "'gives' must be an array of {goal, expression} objects"

  parseGiveItem :: Value -> Either Text GiveItem
  parseGiveItem (Aeson.Object fields) = do
    goal <- case KeyMap.lookup "goal" fields of
      Just value -> case Aeson.fromJSON value of
        Aeson.Success n -> Right (InteractionId n)
        Aeson.Error _ -> Left "A 'gives' entry has a non-integer 'goal'"
      Nothing -> Left "A 'gives' entry is missing 'goal'"
    expression <- case KeyMap.lookup "expression" fields of
      Just (Aeson.String s) -> Right (Text.strip s)
      Just _ -> Left "A 'gives' entry has a non-string 'expression'"
      Nothing -> Left "A 'gives' entry is missing 'expression'"
    when (Text.null expression) $
      Left "A 'gives' entry has an empty 'expression'"
    pure (goal, (Text.unpack expression))
  parseGiveItem _ = Left "Each 'gives' entry must be a {goal, expression} object"

-- Run each give against the current loaded state in order, accumulating the
-- edits. The gives don't touch the file yet, so every edit's span stays valid
-- against the current source. If any give fails to typecheck, we stop, roll
-- back the in-flight gives by reloading the (untouched) file, and report the
-- failure. If each give is successful, we write the accumulated edits back to
-- disk.
give :: Worker -> GiveRequest -> IO GiveResponse
give worker (GiveRequest path items) =
  runExceptT (traverse runGive (zip [0 :: Int ..] items))
    >>= either reject commit
 where
  runGive :: (Int, GiveItem) -> ExceptT RejectedGive IO Edit
  runGive (priorCount, (goal, expression)) =
    ExceptT $
      first (flip (RejectedGive goal) priorCount)
        <$> runCommand worker (giveCommand path goal expression)

  -- TODO: Consequence of TODO above saying to merge `RejectedGive` with `GiveRejected`.
  reject :: RejectedGive -> IO GiveResponse
  reject (RejectedGive goal err priorCount) =
    resync (GiveRejected goal err priorCount)

  -- All gives succeeded. Read the source, confirm every recorded span still
  -- holds a hole (if not, the file change under us since the last load, so
  -- refuse rather than corrupt it), splice all the edits in, write, and reload.
  commit :: [Edit] -> IO GiveResponse
  commit edits =
    ( do
        -- Read the source with Agda's parser, so our edit offsets line up with
        -- its `posPos`.
        source <- LazyText.toStrict <$> readTextFile path
        -- We try to check that each of the `editSpan`s correspond to actual
        -- holes. See .scratch/GIVE_STALENESS.md.
        case find (not . spanIsHole . spanText source . editSpan) edits of
          Just stale -> resync (GiveStale stale)
          Nothing -> do
            -- Note: we write UTF-8 with LF endings regardless of platform.
            -- Agda's reader (`readTextFile`) normalizes to LF internally, so
            -- the hole span indices depend on this behavior. I just don't
            -- really care to think about what the defensibly correct solution
            -- is here, so I'm going to say "screw you Windows" and write LFs.
            atomicWriteFile path (encodeUtf8 (applyEdits source edits))
            resync (GiveApplied edits)
    )
      -- Catch `IOException`s (missing file, permissions, disk full) and
      -- `readTextFile`'s decoding `ReadException`.
      `catches` [ Handler (fromIOError :: IOException -> IO GiveResponse)
                , Handler (fromIOError :: ReadException -> IO GiveResponse)
                ]

  -- Every `give` outcome ends by reloading. The happy path uses this to report
  -- the fresh goals, while the sad paths discard any in-memory gives and sync
  -- Agda's state with the contents on disk.
  resync :: (LoadResponse -> a) -> IO a
  resync f = f <$> load worker (LoadRequest path)

  fromIOError :: (Exception e) => e -> IO GiveResponse
  fromIOError = resync . GiveIOError . Text.pack . displayException

-- Either a rendered type error (the give failed) or the edit to splice: the
-- hole's span and the text Agda elaborated the expression to.
giveCommand ::
  FilePath -> InteractionId -> String -> Command (Either Text Edit)
giveCommand path goal expression =
  Command
    { commandIOTCM =
        IOTCM path None Direct (Cmd_give WithoutForce goal noRange expression)
    , commandParse = \responses ->
        either
          (pure . Left)
          (resolveGive goal responses)
          (parseGiveResponses goal responses)
    }

-- An edit directive resulting from the application of a give command,
-- consisting of the interaction id, the hole's span, and the expression text
-- elaborated by Agda.
data Edit = Edit
  { editGoal :: InteractionId
  , editSpan :: Span
  , editText :: Text
  }
  deriving (Show)

resolveGive ::
  InteractionId ->
  [Response] ->
  Either Info_Error String ->
  TCM (Either (ProtocolViolation Response) (Either Text Edit))
resolveGive _ _ (Left e) = Right . Left . Text.pack <$> showInfoError e
resolveGive goal responses (Right s) =
  maybe
    missing
    gave
    . Agda.Syntax.Position.rangeToInterval
    <$> getInteractionRange goal
 where
  gave interval = Right (Right (Edit goal (toSpan interval) (Text.pack s)))
  missing = Left (ProtocolViolation "Cmd_give" responses)

-- TODO: Remove the `Text.strip`?
spanIsHole :: Text -> Bool
spanIsHole raw =
  stripped == "?"
    || ("{!" `Text.isPrefixOf` stripped && "!}" `Text.isSuffixOf` stripped)
 where
  stripped = Text.strip raw

-- Splice from the end of the file backwards so each edit's offsets stay valid:
-- an edit only shifts text after it, and holes never overlap.
applyEdits :: Text -> [Edit] -> Text
applyEdits source edits =
  foldl'
    splice
    source
    (sortBy (comparing (Down . positionOffset . spanStart . editSpan)) edits)
 where
  splice text edit =
    let (before, rest) = Text.splitAt (positionOffset (spanStart (editSpan edit))) text
        after = Text.drop (spanLength (editSpan edit)) rest
     in before <> editText edit <> after

{- The grammar of a Cmd_give response list, following the Agda 2.8.0 source:

exchange := prelude? given
          | prelude? failed

prelude := Status ClearRunningInfo ClearHighlighting RunningInfo*
        -- Appears only when the give targets a not-yet-loaded
        -- file. `runInteraction` executes an implicit cmd_load' with a no-op
        -- continuation (InteractionTop.hs:257-263, cmd_load':848-869).

given := GiveAction (goal, Give_String s) -- give_gen:1021; noRange => Give_String (:1010)
         Status                           -- give_gen ends with `Cmd_metas` => display_info (:1024)
         DisplayInfo (Info_AllGoalsWarnings)  -- (:1145-1146); discarded, superseded by the reload
         InteractionPoints                -- runInteraction:268-271 (updateInteractionPointsAfter Cmd_give)

failed := DisplayInfo (Info_Error)       -- handleErr:216-242, as in matchLoad
          JumpToError?                   -- with noRange, typically absent
          HighlightingInfo
          Status                         -- hardcoded sChecked=False (:239)
-}
parseGiveResponses ::
  InteractionId ->
  [Response] ->
  Either (ProtocolViolation Response) (Either Info_Error String)
parseGiveResponses goal responses = maybe (Left violation) Right (exchange responses)
 where
  violation = ProtocolViolation "Cmd_give" responses

  exchange
    ( Resp_Status status : Resp_ClearRunningInfo
        : Resp_ClearHighlighting NotOnlyTokenBased
        : rest
      )
      | not (sChecked status) = afterPrelude rest
  exchange rest = afterPrelude rest

  afterPrelude (Resp_RunningInfo {} : rest) = afterPrelude rest
  afterPrelude rest = given rest <|> failed rest

  given
    ( Resp_GiveAction goal' (Give_String s)
        : Resp_Status _
        : Resp_DisplayInfo (Info_AllGoalsWarnings _ _)
        : [Resp_InteractionPoints _]
      )
      | goal' == goal = Just (Right s)
  given _ = Nothing

  failed = failedTail Left

renderGiveResponse :: GiveResponse -> Text
renderGiveResponse (GiveApplied edits reloaded) =
  "Gave "
    <> Text.pack (show (length edits))
    <> " goal(s):\n"
    <> Text.concat
      [ "  ?"
          <> Text.pack (show (interactionId (editGoal e)))
          <> " := "
          <> editText e
          <> "\n"
      | e <- edits
      ]
    <> "The file was updated on disk and reloaded; interaction ids may have changed.\n\n"
    <> renderLoadResponse reloaded
renderGiveResponse (GiveRejected goal err priorCount reloaded) =
  "Give failed for goal ?"
    <> Text.pack (show (interactionId goal))
    <> ":\n"
    <> err
    <> "\n\n"
    <> ( if priorCount > 0
           then
             "The file was left unchanged; the "
               <> Text.pack (show priorCount)
               <> " earlier give(s) in this call were discarded. "
           else "The file was left unchanged. "
       )
    <> "Reloaded to resync:\n\n"
    <> renderLoadResponse reloaded
renderGiveResponse (GiveStale edit reloaded) =
  "Refused to edit: goal ?"
    <> Text.pack (show (interactionId (editGoal edit)))
    <> " no longer points at a hole (expected `?` or `{! !}` at "
    <> renderSpan (editSpan edit)
    <> "). The file likely changed on disk since it was loaded. No changes were \
       \made; reloaded to resync:\n\n"
    <> renderLoadResponse reloaded
renderGiveResponse (GiveIOError err reloaded) =
  "Could not access the file on disk:\n"
    <> err
    <> "\n\nNo changes were written. Reloaded to resync:\n\n"
    <> renderLoadResponse reloaded

-- Positions

-- A position in the loaded file, as both a 0-based code-point offset into the
-- Agda-normalized source text (what `applyEdits` splices with; see the note
-- in `commit` about normalization) and the 1-based line/column that Agda
-- prints. Agda's `posPos` is 1-based, hence the shift in `toPos`.
data Position = Position
  { positionOffset :: Int
  , positionLine :: Int
  , positionColumn :: Int
  }
  deriving (Show)

-- A contiguous part of the loaded file: start inclusive, end exclusive.
data Span = Span
  { spanStart :: Position
  , spanEnd :: Position
  }
  deriving (Show)

toPosition :: Agda.Syntax.Position.PositionWithoutFile -> Position
toPosition p =
  Position
    { positionOffset = fromIntegral (Agda.Syntax.Position.posPos p) - 1
    , positionLine = fromIntegral (Agda.Syntax.Position.posLine p)
    , positionColumn = fromIntegral (Agda.Syntax.Position.posCol p)
    }

toSpan :: Agda.Syntax.Position.IntervalWithoutFile -> Span
toSpan i =
  Span
    (toPosition (Agda.Syntax.Position.iStart i))
    (toPosition (Agda.Syntax.Position.iEnd i))

-- A `Span` is only meaningful against a loaded file, so ranges that lie
-- elsewhere (warnings can point into imported modules) or have no interval
-- convert to `Nothing`.
fileSpan :: AbsolutePath -> Agda.Syntax.Position.Range -> Maybe Span
fileSpan p r = do
  rangeFile <- Strict.toLazy (Agda.Syntax.Position.rangeFile r)
  guard (Agda.Syntax.Position.rangeFilePath rangeFile == p)
  toSpan <$> Agda.Syntax.Position.rangeToInterval r

spanText :: Text -> Span -> Text
spanText t s =
  Text.take
    (spanLength s)
    (Text.drop (positionOffset (spanStart s)) t)

spanLength :: Span -> Int
spanLength s = positionOffset (spanEnd s) - positionOffset (spanStart s)

renderSpan :: Span -> Text
renderSpan s = renderPosition (spanStart s) <> "-" <> renderPosition (spanEnd s)

renderPosition :: Position -> Text
renderPosition (Position _ l c) =
  Text.pack (show l) <> ":" <> Text.pack (show c)

-- Helpers

-- A `Failure` is a bug in agda-mcp, not a runtime exception we should
-- recover. We throw it here at the tool-handler boundary and deliberately catch
-- it nowhere. This causes the process to die and the dump the error to stderr.
runCommand :: Worker -> Command r -> IO r
runCommand worker command =
  sendCommand worker command >>= either throwIO pure

failedTail :: (Info_Error -> a) -> [Response] -> Maybe a
failedTail wrap (Resp_DisplayInfo (Info_Error e) : rest) = case rest of
  [ Resp_JumpToError {}
    , Resp_HighlightingInfo _ KeepHighlighting _ _
    , Resp_Status status
    ]
      | not (sChecked status) -> Just (wrap e)
  [Resp_HighlightingInfo _ KeepHighlighting _ _, Resp_Status status]
    | not (sChecked status) -> Just (wrap e)
  _ -> Nothing
failedTail _ _ = Nothing
