{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.CaseSplit (
  caseSplitTool,
  Request (..),
  Response (..),
  Outcome (..),
  Edit (..),
  ClauseLayout (..),
  caseSplit,
  clauseLayout,
  layoutClauses,
  renderResponse,
) where

import Agda.Syntax.Common (InteractionId (..))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, withObject, (.:), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction (Error (..), Position (..), Span (..))
import AgdaMCP.Interaction.MakeCase (
  MakeCaseError (..),
  MakeCaseReport (..),
  MakeCaseVariant (..),
  Split (..),
 )
import AgdaMCP.Interaction.MakeCase qualified as Interaction.MakeCase
import AgdaMCP.Tools.Description (
  Description (..),
  Effect (..),
  Shape (..),
  Variant (..),
  field,
  goalIdField,
  loadIdField,
  loadIdRefusals,
  parameter,
  timeoutField,
 )
import AgdaMCP.Tools.Json (
  encodeError,
  encodeGoalId,
  encodeLoadIdRefusal,
  encodeSpan,
  kinded,
 )
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (
  CurrentLoad (..),
  LoadId,
  LoadIdRefusal,
  renderLoadIdRefusal,
 )
import AgdaMCP.Tools.Render (
  blocks,
  indent,
  renderFileChanged,
  renderGoalId,
  renderSourceUnreadable,
  renderWarning,
  renderWriteFailed,
  section,
 )
import AgdaMCP.Tools.Source (
  SourceRefusal (..),
  SourceUnwritable (..),
  checkClauseSpan,
  commitEdits,
  readChecked,
 )
import AgdaMCP.Tools.State (ToolM, liftInteraction)
import AgdaMCP.Tools.Tool (
  Budget (..),
  Tool (..),
  editBudget,
  toolHandle,
 )

caseSplitTool :: Tool
caseSplitTool =
  Tool
    { toolDescription = caseSplitDescription
    , toolCall = toolHandle editBudget caseSplit renderResponse
    }

caseSplitDescription :: Description
caseSplitDescription =
  Description
    { descriptionName = "case_split"
    , descriptionSummary =
        "Split an open goal into cases, replacing the clause containing the goal with one clause per case. Pass the pattern variables to split on, or the empty list to introduce the clause's remaining arguments. A variable not currently in scope is revealed as a visible pattern rather than split on. The replacement is written to the file, and a reload follows, which issues a new load ID and fresh goal ID assignments, so the goal IDs in this request are no longer valid afterwards. Do not run other calls against the same load ID while this one runs; they are refused as stale once it finishes. If the call runs out of time after the replacement is written but before the reload finishes, the file on disk has changed without having been checked, and the other tools refuse to work against the earlier load until you call `load` again."
    , descriptionParameters =
        [ loadIdField
        , goalIdField
        , parameter
            "variables"
            "The pattern variables to split on, by name. An empty list instead introduces the clause's remaining arguments."
            (ShapeArray ShapeString)
        , timeoutField (budgetDefault editBudget)
        ]
    , descriptionAnswers =
        [ Variant
            "completed"
            "the outcome, then the reload that followed"
            [ field "goal" ShapeInteger
            , field "outcome" (ShapeSum "OUTCOME" caseSplitOutcomes)
            , field "reload" Load.loadShape
            ]
        ]
    , descriptionRefusals = loadIdRefusals
    , descriptionEffect = Edits
    }

caseSplitOutcomes :: [Variant]
caseSplitOutcomes =
  [ Variant
      "applied"
      "the clause was replaced by the clauses listed"
      [ field "span" (ShapeNamed "SPAN")
      , field "variant" (ShapeEnum ["function", "extended_lambda"])
      , field "layout" (ShapeEnum ["one_per_line", "inline"])
      , field "clauses" (ShapeArray ShapeString)
      , field "collapses_where" ShapeBoolean
      ]
  , Variant
      "unknown_goal"
      "no such goal in the current load, so nothing was written"
      []
  , Variant
      "failed"
      "Agda could not split the goal, so nothing was written"
      [field "error" (ShapeNamed "ERROR")]
  , Variant
      "file_changed"
      "the file on disk no longer matches the source Agda checked, so nothing was written"
      []
  , Variant
      "source_unreadable"
      "the file could not be read, so nothing was written"
      [field "detail" ShapeString]
  , Variant
      "write_failed"
      "the file could not be written, so it is unchanged"
      [field "detail" ShapeString]
  ]

data Request = Request
  { caseSplitRequestLoadId :: LoadId
  , caseSplitRequestGoalId :: InteractionId
  , caseSplitRequestVariables :: [Text]
  {- ^ Empty introduces the clause's remaining arguments, or splits on the
  result if they are already introduced.
  -}
  }
  deriving (Eq, Show)

data Response
  = ResponseRefused LoadIdRefusal
  | ResponseCompleted InteractionId Outcome Load.Response
  deriving (Eq, Show)

data Outcome
  = OutcomeApplied Edit
  | OutcomeUnknownGoal
  | OutcomeFailed Error
  | OutcomeFileChanged
  | OutcomeSourceUnreadable Text
  | OutcomeWriteFailed Text
  deriving (Eq, Show)

data Edit = Edit
  { editSpan :: Span
  , editVariant :: MakeCaseVariant
  , editLayout :: ClauseLayout
  , editClauses :: [Text]
  , editCollapsesWhere :: Bool
  }
  deriving (Eq, Show)

data ClauseLayout = OnePerLine | Inline
  deriving (Eq, Show)

-- Business logic

caseSplit :: Request -> ToolM Response
caseSplit (Request loadId goalId variables) =
  either ResponseRefused (uncurry $ ResponseCompleted goalId)
    <$> Load.withEditableLoad loadId attempt
 where
  attempt :: CurrentLoad -> ToolM Outcome
  attempt current = do
    source <- liftIO $ readChecked path $ currentLoadSourceHash current
    case source of
      Left (RefusalUnreadable message) -> pure $ OutcomeSourceUnreadable message
      Left RefusalChanged -> pure OutcomeFileChanged
      Right original -> runSplit original
   where
    path = currentLoadPath current

    runSplit :: Text -> ToolM Outcome
    runSplit original = do
      response <-
        liftInteraction $
          Interaction.MakeCase.makeCase
            Interaction.MakeCase.Request
              { Interaction.MakeCase.requestGoalId = goalId
              , Interaction.MakeCase.requestSplit =
                  maybe IntroduceArgumentsOrSplitResult SplitVariables $
                    nonEmpty variables
              }
      case response of
        Left MakeCaseUnknownId {} -> pure OutcomeUnknownGoal
        Left (MakeCaseFailed e) -> pure $ OutcomeFailed e
        Right report -> commit original report

    commit :: Text -> MakeCaseReport -> ToolM Outcome
    commit original report = do
      either (liftIO . throwIO) pure $ checkClauseSpan original span'
      written <- liftIO $ commitEdits path original [(span', replacement)]
      pure $ case written of
        Left unwritable ->
          OutcomeWriteFailed $ sourceUnwritableMessage unwritable
        Right () -> OutcomeApplied edit
     where
      span' = makeCaseReportSpan report
      clauses = makeCaseReportClauses report
      layout = clauseLayout original span'
      replacement = layoutClauses layout clauses
      edit =
        Edit
          { editSpan = span'
          , editVariant = makeCaseReportVariant report
          , editLayout = layout
          , editClauses = clauses
          , editCollapsesWhere = makeCaseReportCollapsesWhere report
          }

{- | Which layout the clauses replacing this span should use.

Emacs decides by scanning backwards from the hole for a @;@ or an opening brace
(`agda2-make-case-action-extendlam`, agda2-mode.el:930-953). We have the span,
so the same question is just whether it starts at its line's first
non-whitespace column. That covers ordinary function clauses, @λ where@ and
@λ { }@ alike, so @MakeCaseVariant@ does not drive layout.
-}
clauseLayout :: Text -> Span -> ClauseLayout
clauseLayout source span'
  | Text.all isSpace linePrefix = OnePerLine
  | otherwise = Inline
 where
  linePrefix =
    Text.takeWhileEnd (/= '\n') $
      Text.take (positionOffset $ spanStart span') source

layoutClauses :: ClauseLayout -> [Text] -> Text
layoutClauses OnePerLine = Text.intercalate "\n"
layoutClauses Inline = Text.intercalate " ; "

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "case_split arguments" $ \o ->
    Request
      <$> o .: "load_id"
      <*> (InteractionId <$> o .: "goal")
      <*> Aeson.explicitParseField parseVariables o "variables"

parseVariables :: Aeson.Value -> Aeson.Parser [Text]
parseVariables = Aeson.withArray "variables" $ \values ->
  traverse
    (\(index, value) -> parseVariable value Aeson.<?> Aeson.Index index)
    (zip [0 ..] $ toList values)

parseVariable :: Aeson.Value -> Aeson.Parser Text
parseVariable = Aeson.withText "variable" validateVariable

{- | Reject names that would select a different `Split` than the one asked for.
The wrapper re-encodes the list as the string Agda splits with `words`, so @"."@
would reach the ellipsis branch this tool does not expose, and a name carrying
whitespace would silently become a different number of variables.
-}
validateVariable :: Text -> Aeson.Parser Text
validateVariable name
  | name == "." =
      fail
        "expected a pattern variable name, but got \".\", which Agda reads as \
        \the ellipsis of a with-clause rather than as a variable"
  | Text.words name == [name] = pure name
  | null (Text.words name) =
      fail $
        "expected a pattern variable name, but got " <> show name
  | length (Text.words name) == 1 =
      fail $
        "expected a pattern variable name with no surrounding whitespace, but \
        \got "
          <> show name
  | otherwise =
      fail $
        "expected a single pattern variable name, but got "
          <> show name
          <> ", which is "
          <> show (length $ Text.words name)
          <> " names; send each as its own entry"

-- Response rendering

renderResponse :: Response -> Either Text Text
renderResponse (ResponseRefused refusal) = Left $ renderLoadIdRefusal refusal
renderResponse (ResponseCompleted goalId outcome reload) =
  Right $ blocks [renderOutcome goalId outcome, Load.renderResponse reload]

renderOutcome :: InteractionId -> Outcome -> Text
renderOutcome goalId (OutcomeApplied edit) =
  blocks $
    section
      ( "Replaced the clause at "
          <> renderGoalId goalId
          <> " with "
          <> countClauses (length $ editClauses edit)
          <> ":"
      )
      [indent $ layoutClauses (editLayout edit) (editClauses edit)]
      <> [whereCollapsed | editCollapsesWhere edit]
renderOutcome goalId OutcomeUnknownGoal =
  renderRefusal
    goalId
    ( indent
        "There is no such goal in the current load. Check the goal IDs in the \
        \most recent result."
    )
    []
renderOutcome goalId (OutcomeFailed e) =
  renderRefusal
    goalId
    (indent $ errorMessage e)
    (map renderWarning $ errorWarnings e)
renderOutcome _ OutcomeFileChanged = renderFileChanged
renderOutcome _ (OutcomeSourceUnreadable message) = renderSourceUnreadable message
renderOutcome _ (OutcomeWriteFailed message) = renderWriteFailed message

renderRefusal :: InteractionId -> Text -> [Text] -> Text
renderRefusal goalId reason warnings =
  blocks $ [header, reason] <> section "Warnings:" warnings
 where
  header = "Cannot split " <> renderGoalId goalId <> ". No edits were written."

whereCollapsed :: Text
whereCollapsed =
  "Warning: the `where` block that followed the clause you split now belongs \
  \to the last of the new clauses alone, so the earlier clauses cannot see its \
  \bindings. If they use those bindings, the reload below reports the errors. \
  \Lift the bindings to the enclosing module or give each clause its own \
  \`where` block."

countClauses :: Int -> Text
countClauses 1 = "1 clause"
countClauses n = Text.pack (show n) <> " clauses"

instance ToJSON Response where
  toJSON (ResponseRefused refusal) = encodeLoadIdRefusal refusal
  toJSON (ResponseCompleted goalId outcome reload) =
    kinded
      "completed"
      [ "goal" .= encodeGoalId goalId
      , "outcome" .= encodeOutcome outcome
      , "reload" .= reload
      ]

encodeOutcome :: Outcome -> Value
encodeOutcome (OutcomeApplied edit) =
  kinded
    "applied"
    [ "span" .= encodeSpan (editSpan edit)
    , "variant" .= renderVariant (editVariant edit)
    , "layout" .= renderLayout (editLayout edit)
    , "clauses" .= editClauses edit
    , "collapses_where" .= editCollapsesWhere edit
    ]
encodeOutcome OutcomeUnknownGoal = kinded "unknown_goal" []
encodeOutcome (OutcomeFailed e) = kinded "failed" ["error" .= encodeError e]
encodeOutcome OutcomeFileChanged = kinded "file_changed" []
encodeOutcome (OutcomeSourceUnreadable detail) =
  kinded "source_unreadable" ["detail" .= detail]
encodeOutcome (OutcomeWriteFailed detail) =
  kinded "write_failed" ["detail" .= detail]

renderVariant :: MakeCaseVariant -> Text
renderVariant MakeCaseFunction = "function"
renderVariant MakeCaseExtendedLambda = "extended_lambda"

renderLayout :: ClauseLayout -> Text
renderLayout OnePerLine = "one_per_line"
renderLayout Inline = "inline"
