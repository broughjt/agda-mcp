{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AgdaMCP (
  capabilities,
  handlers,
  implementation,
  instructions,
  newToolState,
) where

import AgdaMCP.Tools (ToolState, loadTool, newToolState)

import Data.Text (Text)
import MCP.Server (
  Implementation (..),
  MCPHandlerState,
  MCPHandlerUser,
  ProcessHandlers,
  ServerCapabilities (..),
  ToolsCapability (..),
  defaultProcessHandlers,
  withToolHandlers,
 )

type instance MCPHandlerState = ToolState
type instance MCPHandlerUser = ()

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
handlers = withToolHandlers [loadTool] defaultProcessHandlers
