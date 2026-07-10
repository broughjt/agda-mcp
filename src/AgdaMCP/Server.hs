{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Server (runServer) where

import MCP.Server
import System.IO (BufferMode (..), hSetBuffering, stderr, stdin, stdout)

import AgdaMCP.Session (Session)
import AgdaMCP.Tools (giveTool, loadTool)

runServer :: Session -> IO ()
runServer session = do
  hSetBuffering stderr LineBuffering
  let implementation = Implementation "agda-mcp" "0.1.0.0" (Just "Agda MCP Server")
      instructions =
        "Interact with Agda: `load` loads and typechecks a file, \
        \reporting its goals or errors; `give` fills one or more goals \
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
