{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- MCPHandlerState/MCPHandlerUser are open type families the mcp library
-- requires every application to instantiate; the instances are necessarily
-- orphans (the library's own example does the same).
{-# OPTIONS_GHC -Wno-orphans #-}

module AgdaMCP.Server (runServer) where

import MCP.Server
import System.IO (BufferMode (..), hSetBuffering, stderr, stdin, stdout)

import AgdaMCP.Tools.Give (giveTool)
import AgdaMCP.Tools.Load (loadTool)
import AgdaMCP.Worker (Worker)

type instance MCPHandlerState = ()
type instance MCPHandlerUser = ()

runServer :: Worker -> IO ()
runServer worker = do
  hSetBuffering stderr LineBuffering
  let implementation = Implementation "agda-mcp" "0.1.0.0" (Just "Agda MCP Server")
      instructions =
        "Interact with Agda: agda_load loads and typechecks a file, \
        \reporting its goals or errors; agda_give fills one or more goals \
        \with expressions, updating the file on disk and reloading."
      capabilities =
        ServerCapabilities
          { logging = Nothing
          , prompts = Nothing
          , resources = Nothing
          , tools = Just ToolsCapability {listChanged = Nothing}
          , completions = Nothing
          , experimental = Nothing
          }
      handlers = withToolHandlers [loadTool worker, giveTool worker] defaultProcessHandlers
  serveStdio stdin stdout $
    initMCPServerState
      ()
      Nothing
      Nothing
      capabilities
      implementation
      (Just instructions)
      handlers
