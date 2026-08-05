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
import Control.Monad.State (gets)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )

import AgdaMCP.Interaction (
  Error (..),
  GoalError (..),
  GoalReport (..),
 )
import AgdaMCP.Interaction.Goal qualified as Interaction.Goal
import AgdaMCP.Tools.LoadId (
  LoadId,
  LoadIdRefusal,
  renderLoadIdRefusal,
  requireCurrentLoad,
 )
import AgdaMCP.Tools.MCP (
  goalIdSchema,
  loadIdSchema,
  normalizationSchema,
  parseNormalizationField,
  renderNormalization,
  textToolHandle,
 )
import AgdaMCP.Tools.Render (
  blocks,
  indent,
  renderContextEntry,
  renderShape,
  renderWarning,
  section,
 )
import AgdaMCP.Tools.State (ToolM, ToolState (..), liftInteraction)

goalTool :: ToolHandler
goalTool =
  toolHandler
    "goal"
    (Just goalDescription)
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
 where
  goalDescription :: Text
  goalDescription =
    "Inspect an open goal in the currently loaded Agda file without modifying anything. Reports the goal's type, its local context, its cubical boundary when the goal has one, and any unsolved constraints mentioning the goal. Types are rendered at the requested normalization. To try an expression at a goal, use `check` instead."

data Request = Request
  { goalRequestLoadId :: LoadId
  , goalRequestGoalId :: InteractionId
  , goalRequestNormalization :: Rewrite
  }
  deriving (Eq, Show)

data Response
  = ResponseRefused LoadIdRefusal
  | ResponseUnknownGoal InteractionId
  | -- No known reproducer.
    ResponseFailed Error
  | ResponseOk InteractionId Rewrite GoalReport
  deriving (Eq, Show)

-- Business logic

goal :: Request -> ToolM Response
goal request = do
  -- Validation is pure over the load generation and runs before any command.
  generation <- gets toolLoadGeneration
  case requireCurrentLoad generation (goalRequestLoadId request) of
    Left refusal -> pure $ ResponseRefused refusal
    Right _ -> do
      response <-
        liftInteraction $
          Interaction.Goal.goal
            Interaction.Goal.Request
              { Interaction.Goal.requestNormalization =
                  goalRequestNormalization request
              , Interaction.Goal.requestGoalId = goalRequestGoalId request
              }
      pure $ case response of
        Left (GoalUnknownId unknownId) -> ResponseUnknownGoal unknownId
        Left (GoalFailed e) -> ResponseFailed e
        Right report ->
          ResponseOk
            (goalRequestGoalId request)
            (goalRequestNormalization request)
            report

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "goal arguments" $ \o ->
    Request
      <$> o .: "load_id"
      <*> (InteractionId <$> o .: "goal")
      <*> parseNormalizationField o

-- Response rendering

renderResponse :: Response -> Either Text Text
renderResponse (ResponseRefused refusal) = Left $ renderLoadIdRefusal refusal
renderResponse (ResponseUnknownGoal unknownId) =
  Left $
    "No goal ?"
      <> Text.pack (show $ interactionId unknownId)
      <> " in the current load. Check the goal IDs in the most recent result."
renderResponse (ResponseFailed e) =
  Right $
    blocks $
      ["Goal query failed:", indent $ errorMessage e]
        <> section "Warnings:" (map renderWarning $ errorWarnings e)
renderResponse (ResponseOk goalId rewrite report) =
  Right $ renderGoalReport rewrite goalId report

renderGoalReport :: Rewrite -> InteractionId -> GoalReport -> Text
renderGoalReport rewrite goalId report =
  blocks $
    [ Text.intercalate "\n" $
        header
          -- We reverse context entries so they are listed innermost-first.
          : map (indent . renderContextEntry) (reverse $ goalReportContext report)
            <> [indent $ "⊢ " <> renderShape (goalReportShape report)]
    ]
      <> section "Boundary:" (map indent $ goalReportBoundary report)
      <> section "Constraints:" (map indent $ goalReportConstraints report)
 where
  header =
    "?"
      <> Text.pack (show $ interactionId goalId)
      <> " ("
      <> renderNormalization rewrite
      <> ")"
