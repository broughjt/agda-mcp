{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools (loadTool, giveTool) where

import Control.Applicative ((<|>))
import Control.Exception (
  Exception,
  Handler (..),
  IOException,
  catches,
  displayException,
  throwIO,
 )
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable (toList)
import Data.List (find, intercalate, nub, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LazyText
import System.AtomicWrite.Writer.ByteString (atomicWriteFile)

import Agda.Interaction.Base (
  IOTCM' (..),
  Interaction' (Cmd_give, Cmd_load),
  UseForce (WithoutForce),
 )
import Agda.Interaction.EmacsTop (showGoals, showInfoError)
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  GiveResult (..),
  Goals,
  Info_Error,
  RemoveTokenBasedHighlighting (..),
  Response,
  Response_boot (..),
  Status (..),
 )
import Agda.Syntax.Common (InteractionId (..))
import Agda.Syntax.Common.Aspect (TokenBased (..))
import Agda.Syntax.Position (iEnd, iStart, noRange, posPos, rangeToInterval)
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (
  HighlightingLevel (..),
  HighlightingMethod (..),
  WarningsAndNonFatalErrors (..),
 )
import Agda.TypeChecking.Monad.MetaVars (getInteractionRange)
import Agda.TypeChecking.Pretty.Warning (prettyTCWarnings)
import Agda.Utils.IO.UTF8 (ReadException, readTextFile)
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
  = Loaded Text [InteractionId]
  | LoadFailed Text
  | LoadStale
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
    , commandParse = traverse toLoadResponse . parseLoadResponses
    }

renderLoadResponse :: LoadResponse -> Text
renderLoadResponse (Loaded body ids) =
  -- TODO:
  "Load succeeded. Open goals: "
    <> Text.pack (show (length ids))
    <> " (interaction ids "
    <> Text.pack (show (map interactionId ids))
    <> ").\n"
    <> body
renderLoadResponse (LoadFailed e) = "Load failed:\n" <> e
renderLoadResponse LoadStale =
  "The file changed on disk while Agda was checking it, so the result \
  \was discarded. Please load the file again."

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

toLoadResponse :: LoadResponse' -> TCM LoadResponse
toLoadResponse (LoadGoals goals warnings ids) =
  renderBody
    <$> sequenceA
      [ showGoals goals
      , prettyTCWarnings (nonFatalErrors warnings)
      , prettyTCWarnings (tcWarnings warnings)
      ]
 where
  renderBody parts =
    Loaded (Text.pack (intercalate "\n" (filter (not . null) parts))) ids
toLoadResponse (LoadError e) = LoadFailed . Text.pack <$> showInfoError e
toLoadResponse LoadNotRegistered = pure LoadStale

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

-- Run each give against the current loaded state, accumulating the edits. The
-- gives never touch the file, so every edit's span stays valid against the one
-- source we read at commit time. If any give fails to typecheck we stop, roll
-- the in-flight in-memory gives back by reloading the (untouched) file, and
-- report the failure; the whole call is a no-op on disk.
give :: Worker -> GiveRequest -> IO GiveResponse
give worker (GiveRequest path items) = attempt [] items
 where
  attempt done [] = commit worker path (reverse done)
  attempt done ((goal, expression) : rest) = do
    result <- runCommand worker (giveCommand path goal expression)
    case result of
      Left err -> do
        reloaded <- load worker (LoadRequest path)
        pure (GiveRejected goal err (length done) reloaded)
      Right edit ->
        attempt (edit : done) rest

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

-- An edit directive resulting from the application of a give commands,
-- consisting of the interaction id, the code-point span (0-indexed and
-- end-exclusive), and the expression text elaborated by Agda.
data Edit = Edit
  { editGoal :: InteractionId
  , editStart :: Int
  , editEnd :: Int
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
    . rangeToInterval
    <$> getInteractionRange goal
 where
  gave interval =
    Right
      ( Right
          ( Edit
              goal
              (offset (iStart interval))
              (offset (iEnd interval))
              (Text.pack s)
          )
      )
  missing = Left (ProtocolViolation "Cmd_give" responses)
  offset position = fromIntegral (posPos position) - 1

-- All gives succeeded in memory. Read the source once, confirm every recorded
-- span still holds a hole (if not, the file changed under us since the load, so
-- refuse rather than corrupt it), splice all the edits, write, and reload.
commit :: Worker -> FilePath -> [Edit] -> IO GiveResponse
commit worker path edits =
  ( do
      -- Read the source exactly as Agda's parser does, so our code-point
      -- offsets line up with its `posPos`.
      source <- LazyText.toStrict <$> readTextFile path
      -- We try to check that each of the `editSpan`s correspond to actual
      -- holes. See .scratch/GIVE_STALENESS.md.
      case find (not . spanIsHole . editSpan source) edits of
        Just stale -> resync (GiveStale stale)
        Nothing -> do
          -- Note: we write UTF-8 with LF endings regardless of platform. Agda's
          -- reader (`readTextFile`) normalizes to LF internally, so the hole
          -- span indices depend on this behavior. I just don't really care to
          -- think about what the defensibly correct solution is here, so I'm
          -- going to say "screw you Windows" and write LFs.
          resync (GiveApplied edits)
  )
    -- Catch `IOException`s -- (missing file, permissions, disk full) and
    -- `readTextFile`'s decoding `ReadException`.
    `catches` [ Handler (fromIOError :: IOException -> IO GiveResponse)
              , Handler (fromIOError :: ReadException -> IO GiveResponse)
              ]
 where
  -- Every (non-exception) `commit` outcome ends by reloading. The happy path
  -- uses this to report the fresh goals, while the two sad paths (stale, I/O
  -- failure) discard the attempted gives and sync Agda's state with the
  -- contents on disk.
  resync :: (LoadResponse -> a) -> IO a
  resync f = f <$> load worker (LoadRequest path)

  fromIOError :: (Exception e) => e -> IO GiveResponse
  fromIOError = resync . GiveIOError . Text.pack . displayException

editSpan :: Text -> Edit -> Text
editSpan source edit =
  Text.take (editEnd edit - editStart edit) (Text.drop (editStart edit) source)

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
  foldl' splice source (sortBy (comparing (Down . editStart)) edits)
 where
  splice text (Edit _ start end replacement) =
    let (before, rest) = Text.splitAt start text
        after = Text.drop (end - start) rest
     in before <> replacement <> after

{- The grammar of a Cmd_give response list, following the Agda 2.8.0 source:

exchange := prelude? given
          | prelude? failed

prelude := Status ClearRunningInfo ClearHighlighting RunningInfo*
        -- Appears only when the give targets a not-yet-loaded
        -- file. `runInteraction` executes an implicit cmd_load' with a no-op
        -- continuation (InteractionTop.hs:257-263, cmd_load':848-869).

given := GiveAction (goal, Give_String s) -- give_gen:1021; noRange => Give_String (:1010)
         Status                           -- give_gen ends with Cmd_metas => display_info (:1024)
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
renderGiveResponse (GiveStale (Edit goal start end _) reloaded) =
  "Refused to edit: goal ?"
    <> Text.pack (show (interactionId goal))
    <> " no longer points at a hole (expected `?` or `{! !}` at characters "
    <> Text.pack (show start)
    <> "-"
    <> Text.pack (show end)
    <> "). The file likely changed on disk since it was loaded. No changes were \
       \made; reloaded to resync:\n\n"
    <> renderLoadResponse reloaded
renderGiveResponse (GiveIOError err reloaded) =
  "Could not access the file on disk:\n"
    <> err
    <> "\n\nNo changes were written. Reloaded to resync:\n\n"
    <> renderLoadResponse reloaded

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
