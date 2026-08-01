{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AgdaMCP.Tools.State (
  ToolState (..),
  ToolM,
  newToolState,
  liftInteraction,
  runToolM,
) where

import Agda.Interaction.Options (defaultOptions)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT (..), get, put)
import MCP.Server (
  MCPHandlerState,
  MCPHandlerUser,
  MCPServerState (..),
  MCPServerT,
 )

import AgdaMCP.Interaction (
  InteractionM,
  InteractionState,
  newInteractionState,
 )
import AgdaMCP.Tools.LoadId (LoadGeneration (..))

type instance MCPHandlerState = ToolState
type instance MCPHandlerUser = ()

-- Tool state, consisting of interaction-layer state, held opaquely, and state
-- for tracking load generations.
data ToolState = ToolState
  { toolInteractionState :: InteractionState
  , toolLoadGeneration :: LoadGeneration
  }

type ToolM = StateT ToolState IO

-- TODO: Take Agda command-line configuration
newToolState :: IO ToolState
newToolState =
  flip ToolState LoadGeneration {loadsIssued = 0, currentLoad = Nothing}
    <$> newInteractionState defaultOptions

-- Run an interaction-layer action on the session inside `ToolState`, writing
-- the successor session back.
liftInteraction :: InteractionM a -> ToolM a
liftInteraction action = StateT $ \state -> do
  (result, interactionState) <- runStateT action $ toolInteractionState state
  pure (result, state {toolInteractionState = interactionState})

-- Run a `ToolM` action against the state stored in the MCP server state,
-- storing the successor state back. The transports layer will never run two
-- handlers concurrently (stdio is a serial loop, while HTTP runs each handler
-- inside an `modifyMVar` over the whole server state), so the get/run/put
-- sequence is atomic.
--
-- A bug exception thrown mid-action skips the put and propagates out of the
-- handler, killing the process. We deliberately catch it nowhere, so a tool
-- call that dies discards its whole state change.
runToolM :: ToolM a -> MCPServerT a
runToolM action = do
  state <- get
  (result, toolState) <- liftIO $ runStateT action (mcp_handler_state state)
  put state {mcp_handler_state = toolState}
  pure result
