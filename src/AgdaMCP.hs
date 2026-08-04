{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP (
  capabilities,
  handlers,
  implementation,
  instructions,
  newToolState,
) where

import Data.Text (Text)
import MCP.Server (
  Implementation (..),
  ProcessHandlers,
  ServerCapabilities (..),
  ToolsCapability (..),
  defaultProcessHandlers,
  withToolHandlers,
 )

import AgdaMCP.Tools (
  caseSplitTool,
  checkTool,
  giveTool,
  goalTool,
  loadTool,
  newToolState,
 )

capabilities :: ServerCapabilities
capabilities =
  ServerCapabilities
    { logging = Nothing
    , prompts = Nothing
    , resources = Nothing
    , tools = Just ToolsCapability {listChanged = Nothing}
    , completions = Nothing
    , experimental = Nothing
    }

implementation :: Implementation
implementation = Implementation "agda-mcp" "0.1.0.0" (Just "Agda MCP Server")

instructions :: Text
instructions = "Interact with Agda"

handlers :: ProcessHandlers
handlers =
  withToolHandlers
    [loadTool, goalTool, checkTool, giveTool, caseSplitTool]
    defaultProcessHandlers
