{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Goal (
  goalTool,
  Request (..),
  Response (..),
  goal,
  renderResponse,
  renderGoalReport,
) where

import Agda.Interaction.Base (Rewrite)
import Agda.Syntax.Common (InteractionId (..))
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Map qualified as Map
import Data.Text (Text)
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )

import AgdaMCP.Interaction (Error, GoalReport)
import AgdaMCP.Tools.Internal (
  LoadId,
  LoadIdRefusal,
  ToolM,
  goalIdSchema,
  loadIdSchema,
  normalizationSchema,
  parseNormalizationField,
  textToolHandle,
 )

goalTool :: ToolHandler
goalTool =
  toolHandler
    "goal"
    ( Just ""
    -- TODO:
    -- "Inspect a single open goal in the currently loaded Agda file, without \
    -- \modifying anything. Reports the goal's type, the local context at the \
    -- \goal, its cubical boundary, and any unsolved constraints mentioning \
    -- \it. Types are reported at the requested `normalization`, simplified by \
    -- \default. Goal IDs are only \
    -- \meaningful against the load that issued them, so pass the `load_id` \
    -- \from that load result; an ID from an earlier load is refused. To try \
    -- \an expression at a goal, use `check` instead."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [ ("load_id", loadIdSchema)
              , ("goal", goalIdSchema)
              , ("normalization", normalizationSchema)
              ]
        )
        (Just ["load_id", "goal"])
    )
    (textToolHandle goal renderResponse)

data Request = Request
  { goalRequestLoadId :: LoadId
  , goalRequestGoalId :: InteractionId
  , goalRequestNormalization :: Rewrite
  }

data Response
  = ResponseRefused LoadIdRefusal
  | ResponseUnknownGoal InteractionId
  | -- TODO: Not known the happen in practice
    ResponseFailed Error
  | ResponseOk InteractionId GoalReport
  deriving (Eq, Show)

-- Business logic

goal :: Request -> ToolM Response
goal = error "un"

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "goal arguments" $ \o ->
    Request
      <$> o .: "load_id"
      <*> (InteractionId <$> o .: "goal")
      <*> parseNormalizationField o

-- Response rendering

renderResponse :: Response -> Text
renderResponse = error "un"

-- Shared with the check tool, which reports the same goal alongside its
-- expression results. The context is outermost first with let bindings last --
-- the reverse of the order Agda's own `*Goal type etc.*` buffer displays -- so
-- this is where that order is chosen deliberately.
renderGoalReport :: InteractionId -> GoalReport -> Text
renderGoalReport = error "un"
