{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Server (runServer) where

import AgdaMCP.Session (Session)
import AgdaMCP.Tools (giveTool, loadTool)
import MCP.Server
import System.IO (BufferMode (..), hSetBuffering, stderr, stdin, stdout)

runServer :: Session -> IO ()
runServer session = do
  hSetBuffering stderr LineBuffering
  let capabilities =
        ServerCapabilities
          { logging = Nothing
          , prompts = Nothing
          , resources = Nothing
          , tools = Just ToolsCapability {listChanged = Nothing}
          , completions = Nothing
          , experimental = Nothing
          }
      implementation = Implementation "agda-mcp" "0.1.0.0" (Just "Agda MCP Server")
      instructions =
        -- TODO:
        "Interact with Agda"
      handlers = withToolHandlers [loadTool, giveTool] defaultProcessHandlers
  serveStdio stdin stdout $
    initMCPServerState
      session
      Nothing
      Nothing
      capabilities
      implementation
      (Just instructions)
      handlers
