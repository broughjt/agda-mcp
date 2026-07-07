{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools (loadTool) where

import Control.Exception (throwIO)

import Agda.Interaction.Base (IOTCM' (..), Interaction' (Cmd_load))
import Agda.Interaction.EmacsTop (showGoals, showInfoError)
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  Goals,
  Info_Error,
  RemoveTokenBasedHighlighting (..),
  Response,
  Response_boot (..),
  Status (..),
 )
import Agda.Syntax.Common (InteractionId (..))
import Agda.Syntax.Common.Aspect (TokenBased (..))
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (
  HighlightingLevel (..),
  HighlightingMethod (..),
  WarningsAndNonFatalErrors (..),
 )
import Agda.TypeChecking.Pretty.Warning (prettyTCWarnings)
import AgdaMCP.Worker (
  Command (..),
  ProtocolViolation (ProtocolViolation),
  Worker,
  sendCommand,
 )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.List (intercalate)
import Data.Map (Map)
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

-- Load

newtype LoadRequest = LoadRequest FilePath

data LoadResponse
  = Loaded Text [InteractionId]
  | LoadFailed Text
  | LoadStale
  deriving (Show)

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

-- TODO: Implement this
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
