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
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.=))
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction (
  Error,
  GoalError (..),
  GoalReport (..),
 )
import AgdaMCP.Interaction.Goal qualified as Interaction.Goal
import AgdaMCP.Tools.Description (
  Description (..),
  Effect (..),
  Shape (..),
  Variant (..),
  field,
  goalIdField,
  loadIdField,
  normalizationField,
  normalizationShape,
  timeoutField,
  unknownGoal,
 )
import AgdaMCP.Tools.Json (
  encodeContextEntry,
  encodeError,
  encodeGoalId,
  encodeShape,
  encodeUnknownGoal,
  kinded,
 )
import AgdaMCP.Tools.Load (
  LoadRefusal,
  encodeLoadRefusal,
  loadRefusals,
  renderLoadRefusal,
  requireCheckedLoad,
 )
import AgdaMCP.Tools.LoadId (LoadId)
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
import AgdaMCP.Tools.Tool (
  Budget (..),
  Tool (..),
  inspectBudget,
  parseNormalizationField,
  renderNormalization,
  toolHandle,
 )

goalTool :: Tool
goalTool =
  Tool
    { toolDescription = goalDescription
    , toolCall = toolHandle inspectBudget goal renderResponse
    }

goalDescription :: Description
goalDescription =
  Description
    { descriptionName = "goal"
    , descriptionSummary =
        "Inspect an open goal in the currently loaded Agda file without modifying anything. Reports the goal's type, its local context, its cubical boundary when the goal has one, and any unsolved constraints mentioning the goal. Types are rendered at the requested normalization. To try an expression at a goal, use check or infer instead."
    , descriptionParameters =
        [ loadIdField
        , goalIdField
        , normalizationField
        , timeoutField (budgetDefault inspectBudget)
        ]
    , descriptionAnswers =
        [ Variant
            "goal"
            "the goal's type, context, boundary and constraints"
            [ field "goal" ShapeInteger
            , field "normalization" normalizationShape
            , field "shape" (ShapeNamed "SHAPE")
            , field "context" (ShapeArray (ShapeNamed "ENTRY"))
            , field "boundary" (ShapeArray ShapeString)
            , field "constraints" (ShapeArray ShapeString)
            ]
        , Variant "failed" "Agda rejected the query" [field "error" (ShapeNamed "ERROR")]
        ]
    , descriptionRefusals = loadRefusals <> [unknownGoal]
    , descriptionEffect = Inspects
    }

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

instance ToJSON Response where
  toJSON (ResponseRefused refusal) = encodeLoadRefusal refusal
  toJSON (ResponseUnknownGoal unknownId) = encodeUnknownGoal unknownId
  toJSON (ResponseFailed e) = kinded "failed" ["error" .= encodeError e]
  toJSON (ResponseOk goalId rewrite report) =
    kinded
      "goal"
      [ "goal" .= encodeGoalId goalId
      , "normalization" .= renderNormalization rewrite
      , "shape" .= encodeShape (goalReportShape report)
      , "context" .= map encodeContextEntry (goalReportContext report)
      , "boundary" .= goalReportBoundary report
      , "constraints" .= goalReportConstraints report
      ]
