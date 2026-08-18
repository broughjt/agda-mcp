{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Goal (
  goalTool,
  Request (..),
  Response (..),
  goal,
  renderResponse,
) where

import Agda.Interaction.Base (Rewrite)
import Agda.Syntax.Common (InteractionId (..))
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  SchemaType (..),
  ToolAnnotations (..),
  ToolDefinition (..),
  defaultToolAnnotations,
  mkToolDefinition,
  schema,
 )

import AgdaMCP.Interaction (
  Error,
  GoalError (..),
  GoalReport (..),
 )
import AgdaMCP.Interaction.Goal qualified as Interaction.Goal
import AgdaMCP.Tools.Load (
  LoadRefusal,
  renderLoadRefusal,
  requireCheckedLoad,
 )
import AgdaMCP.Tools.LoadId (LoadId)
import AgdaMCP.Tools.MCP (
  Tool (..),
  goalIdSchema,
  inspectBudget,
  loadIdSchema,
  normalizationSchema,
  parseNormalizationField,
  renderNormalization,
  textToolHandle,
  timeoutSchema,
 )
import AgdaMCP.Tools.Render (
  blocks,
  indent,
  renderAgdaError,
  renderContextEntry,
  renderGoalId,
  renderShape,
  renderUnknownGoal,
  section,
 )
import AgdaMCP.Tools.State (ToolM, liftInteraction)

goalTool :: Tool
goalTool =
  Tool
    { toolDefinition =
        ( mkToolDefinition "goal" goalDescription $
            schema $
              SchemaObject
                [ ("load_id", loadIdSchema)
                , ("goal", goalIdSchema)
                , ("normalization", normalizationSchema)
                , ("timeout", timeoutSchema inspectBudget)
                ]
                ["load_id", "goal"]
        )
          { toolDefinitionAnnotations = Just goalAnnotations
          }
    , toolRun = textToolHandle inspectBudget goal renderResponse
    }
 where
  goalAnnotations :: ToolAnnotations
  goalAnnotations =
    defaultToolAnnotations
      { toolReadOnlyHint = Just True
      , toolIdempotentHint = Just True
      , toolOpenWorldHint = Just False
      }

  goalDescription :: Text
  goalDescription =
    "Inspect an open goal in the currently loaded Agda file without modifying anything. Reports the goal's type, its local context, its cubical boundary when the goal has one, and any unsolved constraints mentioning the goal. Types are rendered at the requested normalization. To try an expression at a goal, use check or infer instead."

data Request = Request
  { goalRequestLoadId :: LoadId
  , goalRequestGoalId :: InteractionId
  , goalRequestNormalization :: Rewrite
  }
  deriving (Eq, Show)

data Response
  = ResponseRefused LoadRefusal
  | ResponseUnknownGoal InteractionId
  | -- No known reproducer.
    ResponseFailed Error
  | ResponseOk InteractionId Rewrite GoalReport
  deriving (Eq, Show)

-- Business logic

goal :: Request -> ToolM Response
goal request =
  requireCheckedLoad (goalRequestLoadId request)
    >>= either (pure . ResponseRefused) (const query)
 where
  query :: ToolM Response
  query = do
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
renderResponse (ResponseRefused refusal) = Left $ renderLoadRefusal refusal
renderResponse (ResponseUnknownGoal unknownId) =
  Left $ renderUnknownGoal unknownId
renderResponse (ResponseFailed e) = Right $ renderAgdaError "Goal query failed:" e
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
    renderGoalId goalId <> " (" <> renderNormalization rewrite <> ")"
