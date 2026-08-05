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
instructions =
  "Tools for developing Agda proofs interactively. Use `load` to type-check a file and see its open goals, inspect those goals with `goal` and `check`, and fill them with `give` and `case_split`. Agda keeps only one file loaded at a time, and each load reports a load ID that the other tools require. `goal` and `check` do not modify anything, while `give` and `case_split` modify the file on disk and reload it, which issues a new load ID and fresh goal ID assignments."

handlers :: ProcessHandlers
handlers =
  withToolHandlers
    [loadTool, goalTool, checkTool, giveTool, caseSplitTool]
    defaultProcessHandlers
