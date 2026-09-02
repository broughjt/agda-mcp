{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Infer (
  inferTool,
  Request (..),
  Response (..),
  InferReport (..),
  infer,
  renderResponse,
) where

import Agda.Interaction.Base (Rewrite)
import Agda.Syntax.Common (InteractionId (..))
import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  SchemaType (..),
  ToolAnnotations (..),
  ToolDefinition (..),
  defaultToolAnnotations,
  describedSchema,
  mkToolDefinition,
  schema,
 )

import AgdaMCP.Interaction (Error, GoalError (..), GoalReport (..))
import AgdaMCP.Interaction.GoalInfer (Have (..))
import AgdaMCP.Interaction.GoalInfer qualified as Interaction.GoalInfer
import AgdaMCP.Tools.Json (encodeError, encodeGoalId, encodeUnknownGoal, kinded)
import AgdaMCP.Tools.Load (
  LoadRefusal,
  encodeLoadRefusal,
  renderLoadRefusal,
  requireCheckedLoad,
 )
import AgdaMCP.Tools.LoadId (LoadId)
import AgdaMCP.Tools.MCP (
  goalIdSchema,
  loadIdSchema,
  normalizationSchema,
  timeoutSchema,
 )
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
  Tool (..),
  inspectBudget,
  parseNormalizationField,
  toolHandle,
 )

inferTool :: Tool
inferTool =
  Tool
    { toolDescription = error "not implemented"
    , toolDefinition =
        ( mkToolDefinition "infer" inferDescription $
            schema $
              SchemaObject
                [ ("load_id", loadIdSchema)
                , ("goal", goalIdSchema)
                , ("normalization", normalizationSchema)
                ,
                  ( "expression"
                  , describedSchema expressionDescription $ SchemaString Nothing
                  )
                , ("timeout", timeoutSchema inspectBudget)
                ]
                ["load_id", "goal", "expression"]
        )
          { toolDefinitionAnnotations = Just inferAnnotations
          }
    , toolCall = toolHandle inspectBudget infer renderResponse
    }
 where
  inferAnnotations :: ToolAnnotations
  inferAnnotations =
    defaultToolAnnotations
      { toolReadOnlyHint = Just True
      , toolIdempotentHint = Just True
      , toolOpenWorldHint = Just False
      }

  inferDescription :: Text
  inferDescription =
    "Infer the type of an expression in the scope of an open goal. Nothing is modified. The response reports the inferred type under \"Have at ?n:\". A failure is a normal result rather than a tool error. Inference does not consult the goal's type, so it can succeed where check fails on the same expression. The expression is parsed and its type inferred in the goal's scope, so its errors carry positions relative to the expression, not the file. Types are rendered at the requested normalization. Several read-only calls may use the same load ID at the same time. To just inspect the goal, use goal instead."

  expressionDescription :: Text
  expressionDescription = "The Agda expression to infer the type of."

data Request = Request
  { inferRequestLoadId :: LoadId
  , inferRequestGoalId :: InteractionId
  , inferRequestNormalization :: Rewrite
  , inferRequestExpression :: Text
  -- ^ Non-blank
  }
  deriving (Eq, Show)

data Response
  = ResponseRefused LoadRefusal
  | ResponseUnknownGoal InteractionId
  | ResponseFailed InteractionId Error
  | ResponseOk InteractionId InferReport
  deriving (Eq, Show)

data InferReport = InferReport
  { inferReportExpression :: Text
  , inferReportHave :: Have
  , inferReportGoalBoundary :: [Text]
  }
  deriving (Eq, Show)

-- Business logic

infer :: Request -> ToolM Response
infer (Request loadId goalId normalization expression) =
  requireCheckedLoad loadId >>= either (pure . ResponseRefused) (const query)
 where
  query :: ToolM Response
  query = do
    response <-
      liftInteraction $
        Interaction.GoalInfer.goalInfer
          Interaction.GoalInfer.Request
            { Interaction.GoalInfer.requestNormalization = normalization
            , Interaction.GoalInfer.requestGoalId = goalId
            , Interaction.GoalInfer.requestExpression = expression
            }
    pure $ case response of
      Left (GoalUnknownId unknownId) -> ResponseUnknownGoal unknownId
      Left (GoalFailed e) -> ResponseFailed goalId e
      Right (report, have) ->
        ResponseOk goalId $
          InferReport
            { inferReportExpression = expression
            , inferReportHave = have
            , inferReportGoalBoundary = goalReportBoundary report
            }

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "infer arguments" $ \o ->
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
      ( "Could not infer the type of the expression at "
          <> renderGoalId goalId
          <> ":"
      )
      e
renderResponse (ResponseOk goalId report) =
  Right $
    blocks $
      section ("Have at " <> renderGoalId goalId <> ":") [inferred]
        <> section
          ("Boundary expected by " <> renderGoalId goalId <> ":")
          (map indent $ inferReportGoalBoundary report)
        <> section
          "Boundary of the expression:"
          (map indent $ haveBoundary have)
 where
  have = inferReportHave report
  inferred =
    indent (inferReportExpression report)
      <> "\n"
      <> indent (indent $ ": " <> haveType have)

instance ToJSON Response where
  toJSON (ResponseRefused refusal) = encodeLoadRefusal refusal
  toJSON (ResponseUnknownGoal unknownId) = encodeUnknownGoal unknownId
  toJSON (ResponseFailed goalId e) =
    kinded "failed" ["goal" .= encodeGoalId goalId, "error" .= encodeError e]
  toJSON (ResponseOk goalId report) =
    kinded
      "inferred"
      [ "goal" .= encodeGoalId goalId
      , "expression" .= inferReportExpression report
      , "type" .= haveType have
      , "boundary" .= haveBoundary have
      , "goal_boundary" .= inferReportGoalBoundary report
      ]
   where
    have = inferReportHave report
