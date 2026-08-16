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
import Data.Aeson (FromJSON (..), withObject, (.:))
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

import AgdaMCP.Interaction (Error)
import AgdaMCP.Tools.Load (LoadRefusal)
import AgdaMCP.Tools.LoadId (LoadId)
import AgdaMCP.Tools.MCP (
  Tool (..),
  goalIdSchema,
  inspectBudget,
  loadIdSchema,
  normalizationSchema,
  parseNormalizationField,
  textToolHandle,
  timeoutSchema,
 )
import AgdaMCP.Tools.State (ToolM)

checkTool :: Tool
checkTool =
  Tool
    { toolDefinition =
        ( mkToolDefinition "check" checkDescription $
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
          { toolDefinitionAnnotations = Just checkAnnotations
          }
    , toolRun = textToolHandle inspectBudget check renderResponse
    }
 where
  checkAnnotations :: ToolAnnotations
  checkAnnotations =
    defaultToolAnnotations
      { toolReadOnlyHint = Just True
      , toolIdempotentHint = Just True
      , toolOpenWorldHint = Just False
      }

  checkDescription :: Text
  checkDescription =
    "Infer the type of an expression and check its elaboration against the type of the goal. Nothing is modified, so use this as a dry run before give. The response reports the goal's type, context, boundary, and constraints, then the inference under \"Have:\" and the elaboration under \"Elaborates to:\". These two queries are independent and answer different questions. Inference does not consult the goal type, so inference can succeed while checking fails. Both are reported whichever way they land, and a failure in either is a normal result rather than a tool error. The expression is parsed and checked in the goal's scope, so its errors carry positions relative to the expression, not the file. Types are rendered at the requested normalization. To just inspect the goal, use goal instead."

  expressionDescription :: Text
  expressionDescription =
    "The Agda expression to infer the type of and to check against the goal."

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
check _ = error "not implemented"

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
renderResponse _ =
  error "not implemented"
