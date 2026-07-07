{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- MCPHandlerState/MCPHandlerUser are open type families the mcp library
-- requires every application to instantiate; the instances are necessarily
-- orphans (the library's own example does the same).
{-# OPTIONS_GHC -Wno-orphans #-}

module AgdaMCP.Server (runServer) where

import Agda.Syntax.Common (InteractionId (..))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server
import System.IO (BufferMode (..), hSetBuffering, stderr, stdin, stdout)

import AgdaMCP.Commands (LoadResult (..), load)
import AgdaMCP.Worker (Command, Worker, sendCommand)

type instance MCPHandlerState = ()
type instance MCPHandlerUser = ()

-- A `Failure` is a bug in agda-mcp, not a runtime exception we should
-- recover. We throw it here at the tool-handler boundary and deliberately catch
-- it nowhere. This causes the process to die and the dump the error to stderr.
runCommand :: Worker -> Command r -> IO r
runCommand worker command =
  sendCommand worker command >>= either throwIO pure

renderLoad :: LoadResult -> Text
renderLoad = \case
  Loaded body ids ->
    "Load succeeded. Open goals: "
      <> Text.pack (show (length ids))
      <> " (interaction ids "
      <> Text.pack (show (map interactionId ids))
      <> ").\n"
      <> body
  LoadFailed err -> "Load failed:\n" <> err
  LoadStale ->
    "The file changed on disk while Agda was checking it, so the result \
    \was discarded. Load the file again."

agdaLoadTool :: Worker -> ToolHandler
agdaLoadTool worker =
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
        (Just $ Map.fromList [("path", pathSchema)])
        (Just ["path"])
    )
    $ \arguments ->
      case arguments >>= Map.lookup "path" of
        Just (Aeson.String path) ->
          liftIO $
            ProcessSuccess . toolTextResult . pure . renderLoad
              <$> runCommand worker (load (Text.unpack path))
        _ ->
          pure $
            ProcessSuccess $
              toolTextError
                "Missing or invalid 'path' argument: expected a string path to an .agda file"
 where
  pathSchema =
    object
      [ "type" .= ("string" :: Text)
      , "description" .= ("Path to the .agda file to load" :: Text)
      ]

runServer :: Worker -> IO ()
runServer worker = do
  hSetBuffering stderr LineBuffering
  let implementation = Implementation "agda-mcp" "0.1.0.0" (Just "Agda MCP Server")
      instructions =
        "Interact with Agda: agda_load loads and typechecks a file, \
        \reporting its goals or errors."
      capabilities =
        ServerCapabilities
          { logging = Nothing
          , prompts = Nothing
          , resources = Nothing
          , tools = Just ToolsCapability {listChanged = Nothing}
          , completions = Nothing
          , experimental = Nothing
          }
      handlers = withToolHandlers [agdaLoadTool worker] defaultProcessHandlers
  serveStdio stdin stdout $
    initMCPServerState
      ()
      Nothing
      Nothing
      capabilities
      implementation
      (Just instructions)
      handlers
