{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Check (
  checkTool,
  Request (..),
  Response (..),
  check,
  renderResponse,
) where

import Agda.Interaction.Base (Rewrite)
import Agda.Syntax.Common (InteractionId (..))
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction (Error, GoalError (..))
import AgdaMCP.Interaction.GoalCheck qualified as Interaction.GoalCheck
import AgdaMCP.Tools.Description (
  Description (..),
  Effect (..),
  Shape (..),
  Variant (..),
  field,
  goalIdField,
  loadIdField,
  normalizationField,
  parameter,
  timeoutField,
  unknownGoal,
 )
import AgdaMCP.Tools.Json (encodeError, encodeGoalId, encodeUnknownGoal, kinded)
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
  renderGoalId,
  renderUnknownGoal,
  section,
 )
import AgdaMCP.Tools.State (ToolM, liftInteraction)
import AgdaMCP.Tools.Tool (
  Budget (..),
  Tool (..),
  inspectBudget,
  parseNormalizationField,
  toolHandle,
 )

checkTool :: Tool
checkTool =
  Tool
    { toolDescription = checkDescription
    , toolCall = toolHandle inspectBudget check renderResponse
    }

checkDescription :: Description
checkDescription =
  Description
    { descriptionName = "check"
    , descriptionSummary =
        -- TODO: Several read-only calls may use the same load ID at the same time.
        "Check an expression against the type of an open goal. Nothing is modified, so use this as a dry run before give. The response states whether the expression checks, and on success reports the elaboration under \"Elaborates to:\". A failure is a normal result rather than a tool error. When the expression does not check, use infer to see its inferred type. The expression is parsed and checked in the goal's scope, so its errors carry positions relative to the expression, not the file. The elaboration is rendered at the requested normalization. Several read-only calls may use the same load ID at the same time. To just inspect the goal, use goal instead."
    , descriptionParameters =
        [ loadIdField
        , goalIdField
        , normalizationField
        , parameter
            "expression"
            "The Agda expression to check against the goal."
            ShapeString
        , timeoutField (budgetDefault inspectBudget)
        ]
    , descriptionAnswers =
        [ Variant
            "checks"
            "the expression checks; its elaboration follows"
            [field "goal" ShapeInteger, field "elaborated" ShapeString]
        , Variant
            "failed"
            "the expression does not check"
            [field "goal" ShapeInteger, field "error" (ShapeNamed "ERROR")]
        ]
    , descriptionRefusals = loadRefusals <> [unknownGoal]
    , descriptionEffect = Inspects
    }

data Request = Request
  { checkRequestLoadId :: LoadId
  , checkRequestGoalId :: InteractionId
  , checkRequestNormalization :: Rewrite
  , checkRequestExpression :: Text
  -- ^ Non-blank
  }
  deriving (Eq, Show)

data Response
  = ResponseRefused LoadRefusal
  | ResponseUnknownGoal InteractionId
  | ResponseFailed InteractionId Error
  | ResponseOk InteractionId Text
  deriving (Eq, Show)

-- Business logic

check :: Request -> ToolM Response
check (Request loadId goalId normalization expression) =
  requireCheckedLoad loadId >>= either (pure . ResponseRefused) (const query)
 where
  query :: ToolM Response
  query = do
    response <-
      liftInteraction $
        Interaction.GoalCheck.goalCheck
          Interaction.GoalCheck.Request
            { Interaction.GoalCheck.requestNormalization = normalization
            , Interaction.GoalCheck.requestGoalId = goalId
            , Interaction.GoalCheck.requestExpression = expression
            }
    pure $ case response of
      Left (GoalUnknownId unknownId) -> ResponseUnknownGoal unknownId
      Left (GoalFailed e) -> ResponseFailed goalId e
      Right (_, elaborated) -> ResponseOk goalId elaborated

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "check arguments" $ \o ->
    Request
      <$> o .: "load_id"
      <*> (InteractionId <$> o .: "goal")
      <*> parseNormalizationField o
      <*> Aeson.explicitParseField parseExpression o "expression"

parseExpression :: Aeson.Value -> Aeson.Parser Text
parseExpression = Aeson.withText "expression" $ \text ->
  if Text.null (Text.strip text)
    then
      fail $
        "expected an Agda expression to try at the goal, but got "
          <> show text
    else pure text

-- Response rendering

renderResponse :: Response -> Either Text Text
renderResponse (ResponseRefused refusal) = Left $ renderLoadRefusal refusal
renderResponse (ResponseUnknownGoal unknownId) =
  Left $ renderUnknownGoal unknownId
renderResponse (ResponseFailed goalId e) =
  Right $
    renderAgdaError
      ("The expression does not check at " <> renderGoalId goalId <> ":")
      e
renderResponse (ResponseOk goalId elaborated) =
  Right $
    blocks $
      ["The expression checks at " <> renderGoalId goalId <> "."]
        <> section "Elaborates to:" [indent elaborated]

instance ToJSON Response where
  toJSON (ResponseRefused refusal) = encodeLoadRefusal refusal
  toJSON (ResponseUnknownGoal unknownId) = encodeUnknownGoal unknownId
  toJSON (ResponseFailed goalId e) =
    kinded "failed" ["goal" .= encodeGoalId goalId, "error" .= encodeError e]
  toJSON (ResponseOk goalId elaborated) =
    kinded "checks" ["goal" .= encodeGoalId goalId, "elaborated" .= elaborated]
