{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools (loadTool, giveTool) where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.List (find, intercalate, nub, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

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
  (LoadRequest . Text.unpack)
    <$> parseTextArgument (fromMaybe Map.empty arguments) "path"

loadCommand :: LoadRequest -> Command LoadResponse
loadCommand (LoadRequest path) =
  Command
    { commandIOTCM = IOTCM path None Direct (Cmd_load path [])
    , commandParse = traverse toLoadResponse . parseLoadResponses
    }

renderLoadResponse :: LoadResponse -> Text
renderLoadResponse (Loaded body ids) =
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
                    , "description" .= ("Path to the loaded .agda file" :: Text)
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
        (liftIO . fmap (ProcessSuccess . toolTextResult . (: [])) . give worker)
        . parseGiveArguments
    )

data GiveRequest = GiveRequest FilePath [GiveItem]

data GiveItem = GiveItem InteractionId String

-- A single applied give: the goal, the code-point span (0-based, end exclusive)
-- it occupied in the normalized source, and the expression Agda elaborated it
-- to (which is what we splice into the file).
data Edit = Edit
  { editGoal :: InteractionId
  , editStart :: Int
  , editEnd :: Int
  , editText :: Text
  }

data GiveReply
  = Gave Int Int Text
  | GiveFailed Text
  deriving (Show)

parseGiveArguments :: Maybe (Map Text Value) -> Either Text GiveRequest
parseGiveArguments arguments = do
  let args = fromMaybe Map.empty arguments
  path <- Text.unpack <$> parseTextArgument args "path"
  givesValue <-
    maybe (Left "Missing 'gives' argument: expected an array") Right (Map.lookup "gives" args)
  items <- parseGiveItems givesValue
  when (null items) $
    Left "The 'gives' array must contain at least one goal/expression pair"
  when (hasDuplicates [g | GiveItem g _ <- items]) $
    Left "Duplicate goal ids in 'gives'; each goal may be given only once per call"
  pure (GiveRequest path items)

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
  pure (GiveItem goal (Text.unpack expression))
parseGiveItem _ = Left "Each 'gives' entry must be a {goal, expression} object"

-- Run each give against the current loaded state, accumulating the edits. The
-- gives never touch the file, so every edit's span stays valid against the one
-- source we read at commit time. If any give fails to typecheck we stop, roll
-- the in-flight in-memory gives back by reloading the (untouched) file, and
-- report the failure; the whole call is a no-op on disk.
give :: Worker -> GiveRequest -> IO Text
give worker (GiveRequest path items) = attempt [] items
 where
  attempt done [] = commit worker path (reverse done)
  attempt done (GiveItem ii expression : rest) = do
    reply <- runCommand worker (giveCommand path ii expression)
    case reply of
      GiveFailed err -> do
        reloaded <- reloadRendered worker path
        pure (renderGiveFailed ii err (length done) reloaded)
      Gave start end text ->
        attempt (Edit ii start end text : done) rest

giveCommand :: FilePath -> InteractionId -> String -> Command GiveReply
giveCommand path ii expression =
  Command
    { commandIOTCM = IOTCM path None Direct (Cmd_give WithoutForce ii noRange expression)
    , commandParse = \responses ->
        case matchGive ii responses of
          Left violation -> pure (Left violation)
          Right (MatchFailed e) -> Right . GiveFailed . Text.pack <$> showInfoError e
          Right (MatchGave s) -> do
            -- Agda never moves an interaction point's range, so this still
            -- reports the hole's position in the originally loaded source.
            interval <- rangeToInterval <$> getInteractionRange ii
            pure $ case interval of
              Just iv ->
                Right (Gave (offset (iStart iv)) (offset (iEnd iv)) (Text.pack s))
              Nothing -> Left (ProtocolViolation "Cmd_give" responses)
    }
 where
  offset position = fromIntegral (posPos position) - 1

data GiveMatch
  = MatchGave String
  | MatchFailed Info_Error

{- The grammar of a Cmd_give response list, following the Agda 2.8.0 source:

exchange := prelude? given
          | prelude? failed

prelude := Status ClearRunningInfo ClearHighlighting RunningInfo*
        -- Only when the give targets a not-yet-loaded file: runInteraction runs
        -- an implicit cmd_load' with a no-op continuation
        -- (InteractionTop.hs:257-263, cmd_load':848-869), so a load prelude and
        -- checking noise but no DisplayInfo and no InteractionPoints.

given := GiveAction (ii, Give_String s)  -- give_gen:1021; noRange => Give_String (:1010)
         Status                          -- give_gen ends with Cmd_metas => display_info (:1024)
         DisplayInfo (Info_AllGoalsWarnings)  -- (:1145-1146); discarded, superseded by the reload
         InteractionPoints               -- runInteraction:268-271 (updateInteractionPointsAfter Cmd_give)

failed := DisplayInfo (Info_Error)       -- handleErr:216-242, as in matchLoad
          JumpToError?                   -- with noRange, typically absent
          HighlightingInfo
          Status                         -- hardcoded sChecked=False (:239)
-}
matchGive :: InteractionId -> [Response] -> Either (ProtocolViolation Response) GiveMatch
matchGive ii responses = maybe (Left violation) Right (exchange responses)
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
    ( Resp_GiveAction ii' (Give_String s)
        : Resp_Status _
        : Resp_DisplayInfo (Info_AllGoalsWarnings _ _)
        : [Resp_InteractionPoints _]
      )
      | ii' == ii = Just (MatchGave s)
  given _ = Nothing

  failed = failedTail MatchFailed

-- All gives succeeded in memory. Read the source once, confirm every recorded
-- span still holds a hole (if not, the file changed under us since the load, so
-- refuse rather than corrupt it), splice all the edits, write, and reload.
commit :: Worker -> FilePath -> [Edit] -> IO Text
commit worker path edits = do
  source <- readAgdaSource path
  case find (not . spanIsHole . editSpan source) edits of
    Just stale -> do
      reloaded <- reloadRendered worker path
      pure (renderGiveStale stale reloaded)
    Nothing -> do
      writeAgdaSource path (applyEdits source edits)
      reloaded <- reloadRendered worker path
      pure (renderGiveSucceeded edits reloaded)

editSpan :: Text -> Edit -> Text
editSpan source edit =
  Text.take (editEnd edit - editStart edit) (Text.drop (editStart edit) source)

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

-- Read the source exactly as Agda does, so our code-point offsets line up with
-- its `posPos`: strip a UTF-8 BOM off the bytes, then normalize line endings
-- (Agda.Utils.IO.UTF8.readTextFile / convertLineEndings).
readAgdaSource :: FilePath -> IO Text
readAgdaSource path = normalizeLineEndings . decodeUtf8 . stripBom <$> BS.readFile path
 where
  stripBom bytes = fromMaybe bytes (BS.stripPrefix (BS.pack [0xEF, 0xBB, 0xBF]) bytes)

normalizeLineEndings :: Text -> Text
normalizeLineEndings = Text.map convert . Text.replace "\x000D\n" "\n"
 where
  convert '\x000D' = '\n' -- CR
  convert '\x000C' = '\n' -- FF
  convert '\x0085' = '\n' -- NEXT LINE
  convert '\x2028' = '\n' -- LINE SEPARATOR
  convert '\x2029' = '\n' -- PARAGRAPH SEPARATOR
  convert c = c

-- Writes UTF-8 with LF endings (the normalized text); a CRLF file is rewritten
-- with LF, which is what Agda saw anyway.
writeAgdaSource :: FilePath -> Text -> IO ()
writeAgdaSource path = BS.writeFile path . encodeUtf8

reloadRendered :: Worker -> FilePath -> IO Text
reloadRendered worker path = renderLoadResponse <$> load worker (LoadRequest path)

renderGiveSucceeded :: [Edit] -> Text -> Text
renderGiveSucceeded edits reloaded =
  "Gave "
    <> Text.pack (show (length edits))
    <> " goal(s):\n"
    <> Text.concat ["  ?" <> showId (editGoal e) <> " := " <> editText e <> "\n" | e <- edits]
    <> "The file was updated on disk and reloaded; interaction ids may have changed.\n\n"
    <> reloaded

renderGiveFailed :: InteractionId -> Text -> Int -> Text -> Text
renderGiveFailed ii err priorCount reloaded =
  "Give failed for goal ?"
    <> showId ii
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
    <> reloaded

renderGiveStale :: Edit -> Text -> Text
renderGiveStale (Edit ii start end _) reloaded =
  "Refused to edit: goal ?"
    <> showId ii
    <> " no longer points at a hole (expected `?` or `{! !}` at characters "
    <> Text.pack (show start)
    <> "-"
    <> Text.pack (show end)
    <> "). The file likely changed on disk since it was loaded. No changes were \
       \made; reloaded to resync:\n\n"
    <> reloaded

showId :: InteractionId -> Text
showId = Text.pack . show . interactionId

-- Helpers

-- A `Failure` is a bug in agda-mcp, not a runtime exception we should
-- recover. We throw it here at the tool-handler boundary and deliberately catch
-- it nowhere. This causes the process to die and the dump the error to stderr.
runCommand :: Worker -> Command r -> IO r
runCommand worker command =
  sendCommand worker command >>= either throwIO pure

parseTextArgument :: Map Text Value -> Text -> Either Text Text
parseTextArgument arguments key = case Map.lookup key arguments of
  Just (Aeson.String value) -> Right value
  _ -> Left ("Missing or invalid '" <> key <> "' argument: expected a string")

hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

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
