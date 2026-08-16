{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP (
  handlers,
  serverInfo,
  newToolState,
) where

import Control.Concurrent.MVar (MVar)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import MCP.Server (
  Error (..),
  McpServerHandlers (..),
  McpServerInfo (..),
  ToolDefinition (..),
  noHandlers,
 )

import AgdaMCP.Tools (
  caseSplitTool,
  checkTool,
  giveTool,
  goalTool,
  loadTool,
  newToolState,
 )
import AgdaMCP.Tools.MCP (Tool (..))
import AgdaMCP.Tools.State (ToolState)

serverInfo :: McpServerInfo
serverInfo =
  McpServerInfo
    { serverName = "agda-mcp"
    , serverVersion = "0.1.0.0"
    , serverInstructions = instructions
    }

instructions :: Text
instructions =
  "Tools for developing Agda proofs interactively. Use `load` to type-check a file and see its open goals, inspect those goals with `goal` and `check`, and fill them with `give` and `case_split`. Agda keeps only one file loaded at a time, and each load reports a load ID that the other tools require. `goal` and `check` do not modify anything, while `give` and `case_split` modify the file on disk and reload it, which issues a new load ID and fresh goal ID assignments."

allTools :: [Tool]
allTools = [loadTool, goalTool, checkTool, giveTool, caseSplitTool]

handlers :: MVar ToolState -> McpServerHandlers
handlers stateVariable =
  noHandlers {tools = Just (list, call)}
 where
  byName :: Map Text Tool
  byName =
    Map.fromList
      [(toolDefinitionName $ toolDefinition tool, tool) | tool <- allTools]

  list _ = pure $ map toolDefinition allTools

  call _ name arguments =
    maybe
      (pure $ Left $ UnknownTool name)
      (\tool -> toolRun tool stateVariable arguments)
      (Map.lookup name byName)
