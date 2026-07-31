{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AgdaMCP.Tools.Internal (
  ToolState (..),
  ToolM,
  LoadId (..),
  LoadGeneration (..),
  CurrentLoad (..),
  currentLoadId,
  newToolState,
  liftInteraction,
  runToolM,
  textToolHandle,
  parseArguments,
  renderSpan,
) where

import Agda.Interaction.Options (defaultOptions)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT (..), get, put)
import Data.Aeson (FromJSON, Value)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  CallToolResult,
  MCPHandlerState,
  MCPHandlerUser,
  MCPServerState (..),
  MCPServerT,
  ProcessResult (..),
  toolTextError,
  toolTextResult,
 )
import Numeric.Natural (Natural)

import AgdaMCP.Interaction (InteractionM, InteractionState, newInteractionState)
import AgdaMCP.Interaction.Model (
  Hash,
  Position (..),
  Span (..),
 )

type instance MCPHandlerState = ToolState
type instance MCPHandlerUser = ()

-- Tool state, consisting of interaction-layer state, held opaquely, and state
-- for tracking load generations.
data ToolState = ToolState
  { toolInteractionState :: InteractionState
  , toolLoadGeneration :: LoadGeneration
  }

type ToolM = StateT ToolState IO

-- Loading replaces Agda's active interaction state and reuses small
-- interaction ids, so a goal id only means something against the load that
-- issued it. This tracks which load that is.
--
-- The count is the single source of the current id (see `currentLoadId`) and is
-- monotonically increasing. A failed or stale load clears `currentLoad` while
-- leaving the count alone, so no id is ever issued twice. We preserve this
-- invariant to prevent the case where we validate stale requests.
data LoadGeneration = LoadGeneration
  { loadsIssued :: Natural
  , currentLoad :: Maybe CurrentLoad
  }

data CurrentLoad = CurrentLoad
  { currentLoadPath :: FilePath
  -- ^ Path of the currently loaded file.
  , currentLoadSourceHash :: Hash
  -- ^ The hash of the contents of the currently loaded file.
  }
  deriving (Eq, Show)

-- A server-issued identifier for one Agda interaction generation the particular
-- current-file state that produced a set of goal ids.
newtype LoadId = LoadId Natural
  deriving (Eq, Show)

-- The id of the current load, if a load is current. Issuing an id increments
-- the count, so the live id is always the most recent one issued.
currentLoadId :: LoadGeneration -> Maybe LoadId
currentLoadId generation =
  LoadId (loadsIssued generation) <$ currentLoad generation

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

textToolHandle ::
  (FromJSON p) =>
  (p -> ToolM q) ->
  (q -> Text) ->
  (Maybe (Map Text Value) -> MCPServerT (ProcessResult CallToolResult))
textToolHandle handle renderResponse =
  either
    (pure . ProcessSuccess . toolTextError)
    ( fmap (ProcessSuccess . toolTextResult . (: []) . renderResponse)
        . runToolM
        . handle
    )
    . parseArguments

-- Rendering helpers

-- A span as line:column, dropping the end line when it repeats the start's.
renderSpan :: Span -> Text
renderSpan s
  | positionLine start == positionLine end =
      renderPosition start <> "-" <> Text.pack (show (positionColumn end))
  | otherwise = renderPosition start <> "-" <> renderPosition end
 where
  start = spanStart s
  end = spanEnd s

renderPosition :: Position -> Text
renderPosition (Position _ l c) =
  -- Offsets are not rendered
  Text.pack (show l) <> ":" <> Text.pack (show c)

-- Tool arguments arrive from the MCP layer as a map of top-level fields.
-- Rebuild the JSON object and decode it with the request type's `FromJSON`
-- instance. A `Left` is agent misuse.
parseArguments :: (FromJSON a) => Maybe (Map Text Value) -> Either Text a
parseArguments =
  first Text.pack
    . Aeson.parseEither Aeson.parseJSON
    . Aeson.Object
    . KeyMap.fromMapText
    . fromMaybe Map.empty
