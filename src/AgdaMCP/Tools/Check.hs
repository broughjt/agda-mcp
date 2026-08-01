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
import Data.Aeson (FromJSON (..), object, withObject, (.:), (.=))
import Data.Map qualified as Map
import Data.Text (Text)
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )

import AgdaMCP.Interaction.GoalInfer (Have (..))
import AgdaMCP.Interaction.Model (Error, GoalReport)
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

checkTool :: ToolHandler
checkTool =
  toolHandler
    "check"
    ( Just ""
    -- TODO:
    -- "Try an expression at an open goal without modifying anything -- the \
    -- \dry run for `give`. Reports the goal (type, context, boundary, \
    -- \constraints), the expression's inferred type (Have), and the \
    -- \expression's elaboration checked against the goal (Checks). Infer and \
    -- \check are independent and answer different questions: inference never \
    -- \consults the goal type, so an expression can infer a type happily and \
    -- \still fail to check against the goal -- which is exactly the \
    -- \diagnostic. Both are reported whichever way they land. The expression \
    -- \is parsed and checked in the goal's scope, so its errors carry \
    -- \positions relative to the expression, not the file. Goal IDs are only \
    -- \meaningful against the load that issued them, so pass that load's \
    -- \`load_id`."
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
  , -- TODO:
    checkRequestExpression :: Text
  {- ^ Non-blank; a blank expression is a class-2 argument error rather than
  the wrapper's business. (The `goal` wrapper is what answers "just show me
  the goal", and `goal` is the tool for it.)
  -}
  }

data Response
  = ResponseRefused LoadIdRefusal
  | ResponseUnknownGoal InteractionId
  | ResponseFailed Error
  | -- The goal the report answers for, beside the report itself, as in
    -- `Goal.ResponseOk`.
    ResponseOk InteractionId CheckReport
  deriving (Eq, Show)

data CheckReport = CheckReport
  { checkReportGoal :: GoalReport
  -- ^ Rendered by `Goal.renderGoalReport`, so the two tools agree.
  , checkReportExpression :: Text
  -- ^ Echoed back, since the rendered Have reads as `<expression> : <type>`.
  , checkReportHave :: Either Error Have
  {- ^ The inferred type of the expression and its actual boundary faces.
  `Left` is always the expression's fault (parse error, out of scope,
  ill-typed) -- doc #1 found `GoalFailed` has no environmental reproducer
  here -- so the rendering should say so rather than blame the environment.
  -}
  , checkReportChecks :: Either Error Text
  {- ^ Agda's elaboration of the expression against the goal type, which is
  Agda's own text and not the caller's (`suc zero` elaborates to `1`).
  -}
  }
  deriving (Eq, Show)

-- Business logic

check :: Request -> ToolM Response
check = error "un"

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "check arguments" $ \o ->
    Request
      <$> o .: "load_id"
      <*> (InteractionId <$> o .: "goal")
      <*> parseNormalizationField o
      <*> o .: "expression"

-- Response rendering

renderResponse :: Response -> Text
renderResponse = error "un"
