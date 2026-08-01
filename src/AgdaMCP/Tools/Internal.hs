{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AgdaMCP.Tools.Internal (
  ToolState (..),
  ToolM,
  LoadId (..),
  LoadGeneration (..),
  CurrentLoad (..),
  LoadIdRefusal (..),
  currentLoadId,
  newToolState,
  liftInteraction,
  runToolM,
  textToolHandle,
  parseArguments,
  renderLoadId,
  normalizations,
  parseNormalizationField,
  loadIdSchema,
  goalIdSchema,
  normalizationSchema,
  renderSpan,
) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Interaction.Options (defaultOptions)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT (..), get, put)
import Data.Aeson (FromJSON (..), Value)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text.Read
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

import AgdaMCP.Interaction (
  Hash,
  InteractionM,
  InteractionState,
  Position (..),
  Span (..),
  newInteractionState,
 )

-- Tool state

type instance MCPHandlerState = ToolState
type instance MCPHandlerUser = ()

-- Tool state, consisting of interaction-layer state, held opaquely, and state
-- for tracking load generations.
data ToolState = ToolState
  { toolInteractionState :: InteractionState
  , toolLoadGeneration :: LoadGeneration
  }

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

type ToolM = StateT ToolState IO

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

-- An identifier which corresponds to the particular current-file state that
-- produced a set of interaction ids.
newtype LoadId = LoadId Natural
  deriving (Eq, Show)

renderLoadId :: LoadId -> Text
renderLoadId (LoadId n) = "L" <> Text.pack (show n)

instance FromJSON LoadId where
  parseJSON = Aeson.withText "load_id" $ \text ->
    case Text.stripPrefix "L" text of
      Just digits
        | Right (n, rest) <- Text.Read.decimal digits
        , Text.null rest ->
            pure $ LoadId n
      _ ->
        fail $
          "expected a load_id from a load result, such as \"L17\", but got "
            <> show text

-- Why a `LoadId` was refused.
data LoadIdRefusal
  = -- Nothing is loaded, so no goal id can mean anything yet.
    NoCurrentLoad
  | -- The submitted id names an earlier load generation. The payload is the
    -- id that *is* current, so the caller can tell how far behind it is.
    StaleLoadId LoadId
  deriving (Eq, Show)

-- Helpers

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

-- Request parsing helpers

-- Tool arguments arrive from the MCP layer as a map of top-level fields.
-- Rebuild the JSON object and decode it with the request type's `FromJSON`
-- instance.
parseArguments :: (FromJSON a) => Maybe (Map Text Value) -> Either Text a
parseArguments =
  first Text.pack
    . Aeson.parseEither Aeson.parseJSON
    . Aeson.Object
    . KeyMap.fromMapText
    . fromMaybe Map.empty

parseNormalization :: Value -> Aeson.Parser Rewrite
parseNormalization =
  Aeson.withText "normalization" $
    maybe
      ( fail $
          "expected one of "
            <> Text.unpack (Text.intercalate ", " $ Map.keys normalizations)
      )
      pure
      . flip Map.lookup normalizations

-- The requested normalization, defaulting the way Agda's own Emacs mode
-- defaults the goal-display commands: `Cmd_goal_type_context` and friends are
-- declared with `agda2-maybe-normalised` (agda2-mode.el:1228-1235), whose
-- no-prefix-argument level is `Simplified`.
parseNormalizationField :: Aeson.Object -> Aeson.Parser Rewrite
parseNormalizationField o =
  fromMaybe Simplified
    <$> Aeson.explicitParseFieldMaybe parseNormalization o "normalization"

normalizations :: Map Text Rewrite
normalizations =
  Map.fromList
    [ ("asis", AsIs)
    , ("instantiated", Instantiated)
    , ("headnormal", HeadNormal)
    , ("simplified", Simplified)
    , ("normalized", Normalised)
    ]

-- Schema elements shared by multiple tools

loadIdSchema :: Value
loadIdSchema =
  Aeson.object
    [ "type" Aeson..= ("string" :: Text)
    , "description"
        -- TODO:
        Aeson..= ( "The `load_id` from the load result that issued this goal \
                   \ID. Goal IDs are renumbered by every load, so an ID from an \
                   \earlier load is refused rather than misread." ::
                     Text
                 )
    ]

goalIdSchema :: Value
goalIdSchema =
  Aeson.object
    [ "type" Aeson..= ("integer" :: Text)
    , "description"
        -- TODO:
        Aeson..= ("The target goal's interaction ID (`?N`) from a load result" :: Text)
    ]

normalizationSchema :: Value
normalizationSchema =
  Aeson.object
    [ "type" Aeson..= ("string" :: Text)
    , "enum" Aeson..= (Map.keys normalizations :: [Text])
    , "description"
        -- TODO:
        Aeson..= ( "How much to normalize the reported goal type and context. \
                   \Defaults to `simplified`, which is what Agda's own goal \
                   \display shows; `normalized` unfolds definitions all the \
                   \way, `asis` reports the types exactly as written." ::
                     Text
                 )
    ]

-- Response rendering helpers

-- Either n:a-b if the start and end lines are the same, or n:a-m:b otherwise.
renderSpan :: Span -> Text
renderSpan s
  | positionLine start == positionLine end =
      renderPosition start <> "-" <> Text.pack (show $ positionColumn end)
  | otherwise = renderPosition start <> "-" <> renderPosition end
 where
  start = spanStart s
  end = spanEnd s

renderPosition :: Position -> Text
renderPosition (Position _ l c) =
  -- Offsets are not rendered
  Text.pack (show l) <> ":" <> Text.pack (show c)
