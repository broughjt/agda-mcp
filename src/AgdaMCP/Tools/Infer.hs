{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Infer (
  Request (..),
  Response (..),
  InferReport (..),
  infer,
  renderResponse,
) where

import Agda.Interaction.Base (Rewrite)
import Agda.Syntax.Common (InteractionId (..))
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction (Error)
import AgdaMCP.Interaction.GoalInfer (Have)
import AgdaMCP.Tools.Load (LoadRefusal)
import AgdaMCP.Tools.LoadId (LoadId)
import AgdaMCP.Tools.MCP (parseNormalizationField)
import AgdaMCP.Tools.State (ToolM)

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
infer _ = error "not implemented"

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
renderResponse _ =
  error "not implemented"
