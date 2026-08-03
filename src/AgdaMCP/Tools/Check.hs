{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Check (
  checkTool,
  Request (..),
  Response (..),
  CheckReport (..),
  check,
  renderResponse,
) where

import Agda.Interaction.Base (Rewrite)
import Agda.Syntax.Common (InteractionId (..))
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import Data.Aeson (FromJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )

import AgdaMCP.Interaction (Error (..), GoalError (..), GoalReport)
import AgdaMCP.Interaction.Goal qualified as Interaction.Goal
import AgdaMCP.Interaction.GoalCheck qualified as Interaction.GoalCheck
import AgdaMCP.Interaction.GoalInfer (Have (..))
import AgdaMCP.Interaction.GoalInfer qualified as Interaction.GoalInfer
import AgdaMCP.Tools.Goal (renderGoalReport)
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
  textToolHandle,
 )
import AgdaMCP.Tools.Render (blocks, indent, renderWarning, section)
import AgdaMCP.Tools.State (ToolM, ToolState (..), liftInteraction)

checkTool :: ToolHandler
checkTool =
  toolHandler
    "check"
    ( Just
        "Try an expression at an open goal without modifying anything — the \
        \dry run for `give`. Reports the goal (type, context, boundary, \
        \constraints), the expression's inferred type (Have), and the \
        \expression's elaboration checked against the goal (Elaborates to). \
        \Infer and check are independent and answer different questions: \
        \inference never consults the goal type, so an expression can infer a \
        \type happily and still fail to check against the goal — which is \
        \exactly the diagnostic. Both are reported whichever way they land. \
        \The expression is parsed and checked in the goal's scope, so its \
        \errors carry positions relative to the expression, not the file. \
        \Types are rendered at the requested `normalization`, `simplified` by \
        \default; `simplified` does not unfold definitions — ask for \
        \`normalized` when you want them unfolded. Goal IDs are only \
        \meaningful against the load that issued them, so pass that load's \
        \`load_id`. To just inspect the goal, use `goal`."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [ ("load_id", loadIdSchema)
              , ("goal", goalIdSchema)
              , ("normalization", normalizationSchema)
              ,
                ( "expression"
                , object
                    [ "type" .= ("string" :: Text)
                    , "description"
                        .= ( "The Agda expression to infer the type of and to \
                             \check against the goal" ::
                               Text
                           )
                    ]
                )
              ]
        )
        (Just ["load_id", "goal", "expression"])
    )
    (textToolHandle check renderResponse)

data Request = Request
  { checkRequestLoadId :: LoadId
  , checkRequestGoalId :: InteractionId
  , checkRequestNormalization :: Rewrite
  , checkRequestExpression :: Text
  -- ^ Non-blank
  }
  deriving (Eq, Show)

data Response
  = ResponseRefused LoadIdRefusal
  | ResponseUnknownGoal InteractionId
  | ResponseFailed Error
  | ResponseOk InteractionId Rewrite CheckReport
  deriving (Eq, Show)

data CheckReport = CheckReport
  { checkReportGoal :: GoalReport
  , checkReportExpression :: Text
  , checkReportHave :: Either Error Have
  , checkReportChecks :: Either Error Text
  }
  deriving (Eq, Show)

-- Business logic

check :: Request -> ToolM Response
check (Request loadId goalId normalization expression) = do
  generation <- gets toolLoadGeneration
  case requireCurrentLoad generation loadId of
    Left refusal -> pure $ ResponseRefused refusal
    Right _ -> do
      goalResponse <-
        liftInteraction $
          Interaction.Goal.goal
            Interaction.Goal.Request
              { Interaction.Goal.requestNormalization = normalization
              , Interaction.Goal.requestGoalId = goalId
              }
      case goalResponse of
        Left (GoalUnknownId unknownId) -> pure $ ResponseUnknownGoal unknownId
        Left (GoalFailed e) -> pure $ ResponseFailed e
        Right report -> do
          haveResponse <-
            liftInteraction $
              Interaction.GoalInfer.goalInfer
                Interaction.GoalInfer.Request
                  { Interaction.GoalInfer.requestNormalization = normalization
                  , Interaction.GoalInfer.requestGoalId = goalId
                  , Interaction.GoalInfer.requestExpression = expression
                  }
          checksResponse <-
            liftInteraction $
              Interaction.GoalCheck.goalCheck
                Interaction.GoalCheck.Request
                  { Interaction.GoalCheck.requestNormalization = normalization
                  , Interaction.GoalCheck.requestGoalId = goalId
                  , Interaction.GoalCheck.requestExpression = expression
                  }
          have <- embedded haveResponse
          checks <- embedded checksResponse
          pure $
            ResponseOk goalId normalization $
              CheckReport
                { checkReportGoal = report
                , checkReportExpression = expression
                , checkReportHave = have
                , checkReportChecks = checks
                }
 where
  embedded :: Either GoalError (GoalReport, a) -> ToolM (Either Error a)
  embedded (Left (GoalUnknownId unknownId)) =
    liftIO $ throwIO $ UnknownGoalAfterResolution unknownId
  embedded (Left (GoalFailed e)) = pure $ Left e
  embedded (Right (_, x)) = pure $ Right x

newtype CheckToolBug = UnknownGoalAfterResolution InteractionId
  deriving (Show)

instance Exception CheckToolBug

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
renderResponse (ResponseRefused refusal) = Left $ renderLoadIdRefusal refusal
renderResponse (ResponseUnknownGoal unknownId) =
  Left $
    "No goal ?"
      <> Text.pack (show $ interactionId unknownId)
      <> " in the current load. Check the goal IDs in the most recent load \
         \result."
renderResponse (ResponseFailed e) =
  Right $
    blocks $
      ["Check failed:", indent $ errorMessage e]
        <> section "Warnings:" (map renderWarning $ errorWarnings e)
renderResponse (ResponseOk goalId rewrite report) =
  Right $
    blocks $
      [renderGoalReport rewrite goalId $ checkReportGoal report]
        <> section "Have:" (either embeddedError haveItems $ checkReportHave report)
        <> section
          "Elaborates to:"
          (either embeddedError (pure . indent) $ checkReportChecks report)
 where
  haveItems (Have inferredType faces) =
    ( indent (checkReportExpression report)
        <> "\n"
        <> indent (indent $ ": " <> inferredType)
    )
      : map indent faces
  embeddedError e =
    indent (errorMessage e) : map renderWarning (errorWarnings e)
