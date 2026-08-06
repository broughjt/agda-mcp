{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Give (
  giveTool,
  Request (..),
  Item,
  Action (..),
  Response (..),
  Outcome (..),
  Edit (..),
  EditKind (..),
  Refusal (..),
  BatchPosition (..),
  RefusalReason (..),
  actions,
  give,
  renderResponse,
) where

import MCP.Server (InputSchema (..), ToolHandler, toolHandler)

import Agda.Interaction.Base (UseForce (..))
import Agda.Syntax.Common (InteractionId (..))
import Control.Exception (throwIO)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), object, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Foldable (toList, traverse_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction (
  Error (..),
  GiveAction (..),
  GiveError (..),
  Span,
 )
import AgdaMCP.Interaction.Give qualified as Interaction.Give
import AgdaMCP.Interaction.Refine qualified as Interaction.Refine
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (
  CurrentLoad (..),
  LoadId,
  LoadIdRefusal,
  renderLoadIdRefusal,
 )
import AgdaMCP.Tools.MCP (goalIdSchema, loadIdSchema, textToolHandle)
import AgdaMCP.Tools.Render (
  blocks,
  indent,
  renderFileChanged,
  renderSourceUnreadable,
  renderWarning,
  renderWriteFailed,
  section,
 )
import AgdaMCP.Tools.Source (
  SourceRefusal (..),
  SourceUnwritable (..),
  checkHole,
  commitEdits,
  readChecked,
 )
import AgdaMCP.Tools.State (ToolM, liftInteraction)

giveTool :: ToolHandler
giveTool =
  toolHandler
    "give"
    (Just giveDescription)
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [ ("load_id", loadIdSchema)
              ,
                ( "gives"
                , object
                    [ "type" .= ("array" :: Text)
                    , "minItems" .= (1 :: Int)
                    , "description" .= givesDescription
                    , "items"
                        .= object
                          [ "type" .= ("object" :: Text)
                          , "properties"
                              .= object
                                [ "goal" .= goalIdSchema
                                , "expression"
                                    .= object
                                      [ "type" .= ("string" :: Text)
                                      , "description" .= expressionDescription
                                      ]
                                , "action"
                                    .= object
                                      [ "type" .= ("string" :: Text)
                                      , "enum" .= (Map.keys actions :: [Text])
                                      , "default" .= ("give" :: Text)
                                      , "description" .= actionDescription
                                      ]
                                ]
                          , "required" .= (["goal", "expression"] :: [Text])
                          ]
                    ]
                )
              ]
        )
        (Just ["load_id", "gives"])
    )
    (textToolHandle give renderResponse)
 where
  giveDescription :: Text
  giveDescription =
    "Fill open goals in the currently loaded Agda file by writing expressions into them. The batch is all-or-nothing and executed in order. If one item is rejected, the rest are skipped and no edits are written. Before writing, the server checks that the file on disk still matches the source Agda checked, and refuses everything if it changed. Except when the load ID is refused, every outcome is followed by a reload that issues a new load ID and fresh goal ID assignments, so the goal IDs in this request are no longer valid afterwards. What is written may be Agda's rendering of the expression rather than the submitted text, so the response shows what went in. Files are written back as UTF-8 with LF line endings."

  givesDescription :: Text
  givesDescription =
    "The batch of goals to fill. Each goal may appear at most once."

  expressionDescription :: Text
  expressionDescription = "The Agda expression to write into the goal."

  actionDescription :: Text
  actionDescription =
    "`give` checks the expression against the goal and writes it, while `refine` partially fills the goal and leaves new subgoals in its place. Defaults to `give`."

data Request = Request
  { giveRequestLoadId :: LoadId
  , giveRequestItems :: [Item]
  -- ^ Non-empty, with no goal id repeated
  }
  deriving (Eq, Show)

type Item = (InteractionId, Action)

-- TODO: Decide whether to add elaborate-give and intro
data Action
  = ActionGive Text
  | ActionRefine Text
  deriving (Eq, Show)

data Response
  = ResponseRefused LoadIdRefusal
  | ResponseCompleted Outcome Load.Response
  deriving (Eq, Show)

data Outcome
  = OutcomeApplied [Edit]
  | OutcomeRefused Refusal
  | OutcomeFileChanged
  | OutcomeSourceUnreadable Text
  | OutcomeWriteFailed Text
  deriving (Eq, Show)

data Edit = Edit
  { editGoalId :: InteractionId
  , editSpan :: Span
  , editText :: Text
  , editKind :: EditKind
  }
  deriving (Eq, Show)

data EditKind = EditKindVerbatim | EditKindParentheses | EditKindComputed
  deriving (Eq, Show)

data Refusal = Refusal
  { refusalGoalId :: InteractionId
  , refusalSpan :: Maybe Span
  , refusalPosition :: BatchPosition
  , refusalReason :: RefusalReason
  }
  deriving (Eq, Show)

-- | Where in the batch the refusal happened. The index is zero-based.
data BatchPosition = BatchPosition {batchIndex :: Int, batchLength :: Int}
  deriving (Eq, Show)

data RefusalReason
  = RefusedUnknownGoal
  | RefusedError Error
  deriving (Eq, Show)

-- Business logic

give :: Request -> ToolM Response
give (Request loadId items) =
  either ResponseRefused (uncurry ResponseCompleted)
    <$> Load.withEditableLoad loadId attempt
 where
  attempt :: CurrentLoad -> ToolM Outcome
  attempt current = do
    source <- liftIO $ readChecked path (currentLoadSourceHash current)
    case source of
      Left (RefusalUnreadable message) -> pure $ OutcomeSourceUnreadable message
      Left RefusalChanged -> pure OutcomeFileChanged
      Right original ->
        runItems >>= either (pure . OutcomeRefused) (commit original)
   where
    path = currentLoadPath current

    commit :: Text -> [Edit] -> ToolM Outcome
    commit original edits = do
      -- The hash matched, so the text on disk is the text Agda checked and
      -- every span is a hole in it. A violation is a bug on our part.
      either (liftIO . throwIO) pure $
        traverse_ (checkHole original . editSpan) edits
      written <-
        liftIO $
          commitEdits path original $
            map (\e -> (editSpan e, editText e)) edits
      pure $ case written of
        Left unwritable ->
          OutcomeWriteFailed $ sourceUnwritableMessage unwritable
        Right () -> OutcomeApplied edits

  runItems :: ToolM (Either Refusal [Edit])
  runItems = runExceptT $ traverse (ExceptT . uncurry runItem) (zip [0 ..] items)

  runItem :: Int -> Item -> ToolM (Either Refusal Edit)
  runItem index (goalId, action) = do
    response <- liftInteraction $ case action of
      ActionGive expression ->
        Interaction.Give.give
          Interaction.Give.Request
            { Interaction.Give.requestForce = WithoutForce
            , Interaction.Give.requestGoalId = goalId
            , Interaction.Give.requestExpression = expression
            }
      ActionRefine expression ->
        Interaction.Refine.refine
          Interaction.Refine.Request
            { Interaction.Refine.requestGoalId = goalId
            , Interaction.Refine.requestExpression = expression
            }
    pure $ case response of
      Left (GiveUnknownId unknownId) ->
        Left $ refused unknownId Nothing RefusedUnknownGoal
      Left (GiveFailed hole e) ->
        Left $ refused goalId (Just hole) (RefusedError e)
      Right (hole, giveAction) ->
        Right $ toEdit goalId (actionExpression action) hole giveAction
   where
    refused unknownId hole reason =
      Refusal
        { refusalGoalId = unknownId
        , refusalSpan = hole
        , refusalPosition = BatchPosition index (length items)
        , refusalReason = reason
        }

actionExpression :: Action -> Text
actionExpression (ActionGive expression) = expression
actionExpression (ActionRefine expression) = expression

toEdit :: InteractionId -> Text -> Span -> GiveAction -> Edit
toEdit goalId expression hole action =
  Edit
    { editGoalId = goalId
    , editSpan = hole
    , editText = text
    , editKind = kind
    }
 where
  (text, kind) = case action of
    GiveVerbatim False -> (expression, EditKindVerbatim)
    GiveVerbatim True -> ("(" <> expression <> ")", EditKindParentheses)
    GiveComputed computed -> (computed, EditKindComputed)

-- Request parsing

instance FromJSON Request where
  parseJSON = Aeson.withObject "give arguments" $ \o ->
    Request
      <$> o .: "load_id"
      <*> Aeson.explicitParseField parseItems o "gives"

actions :: Map Text (Text -> Action)
actions =
  Map.fromList
    [ ("give", ActionGive)
    , ("refine", ActionRefine)
    ]

parseItems :: Aeson.Value -> Aeson.Parser [Item]
parseItems = Aeson.withArray "gives" $ \values -> do
  items <-
    traverse
      (\(index, value) -> parseItem value Aeson.<?> Aeson.Index index)
      (zip [0 ..] $ toList values)
  -- The batch is all-or-nothing and applied in order, so an empty batch has
  -- nothing to do and a repeated goal id would ask us to fill a hole that the
  -- earlier item already solved.
  if null items
    then fail "expected at least one goal to fill, but got none"
    else case repeated (map fst items) of
      Just (InteractionId goalId) ->
        fail $
          "expected each goal to appear at most once, but ?"
            <> show goalId
            <> " appears more than once"
      Nothing -> pure items

repeated :: (Ord a) => [a] -> Maybe a
repeated = snd . foldl' step (Set.empty, Nothing)
 where
  step result@(_, Just _) _ = result
  step (seen, Nothing) item
    | item `Set.member` seen = (seen, Just item)
    | otherwise = (Set.insert item seen, Nothing)

parseItem :: Aeson.Value -> Aeson.Parser Item
parseItem = Aeson.withObject "give item" $ \o -> do
  goalId <- Aeson.explicitParseField parseGoalId o "goal"
  action <- parseActionField o
  expression <- Aeson.explicitParseField parseExpression o "expression"
  pure (goalId, action expression)

parseGoalId :: Aeson.Value -> Aeson.Parser InteractionId
parseGoalId value = do
  goalId <- parseJSON value
  if goalId >= 0
    then pure (InteractionId goalId)
    else fail "expected a non-negative goal ID"

parseActionField :: Aeson.Object -> Aeson.Parser (Text -> Action)
parseActionField o =
  case KeyMap.lookup "action" o of
    Nothing -> pure ActionGive
    Just value ->
      parseAction value Aeson.<?> Aeson.Key "action"

parseAction :: Aeson.Value -> Aeson.Parser (Text -> Action)
parseAction =
  Aeson.withText "action" $
    maybe
      ( fail $
          "expected one of "
            <> Text.unpack (Text.intercalate ", " $ Map.keys actions)
      )
      pure
      . flip Map.lookup actions

parseExpression :: Aeson.Value -> Aeson.Parser Text
parseExpression = Aeson.withText "expression" $ \text ->
  if Text.null (Text.strip text)
    then
      fail $
        "expected an Agda expression to write into the goal, but got "
          <> show text
    else pure text

-- Response rendering

renderResponse :: Response -> Either Text Text
renderResponse (ResponseRefused refusal) = Left $ renderLoadIdRefusal refusal
renderResponse (ResponseCompleted outcome reload) =
  Right $ blocks [renderOutcome outcome, Load.renderResponse reload]

renderOutcome :: Outcome -> Text
renderOutcome (OutcomeApplied edits) =
  blocks $
    section
      ( "Applied "
          <> Text.pack (show $ length edits)
          <> if length edits == 1 then " edit:" else " edits:"
      )
      (map renderEdit edits)
renderOutcome (OutcomeRefused refusal) = renderRefusal refusal
renderOutcome OutcomeFileChanged = renderFileChanged
renderOutcome (OutcomeSourceUnreadable message) = renderSourceUnreadable message
renderOutcome (OutcomeWriteFailed message) = renderWriteFailed message

renderEdit :: Edit -> Text
renderEdit edit =
  indent $
    "?"
      <> Text.pack (show $ interactionId $ editGoalId edit)
      <> " = "
      <> editText edit

renderRefusal :: Refusal -> Text
renderRefusal refusal =
  blocks $
    [header, reason]
      <> section "Warnings:" warnings
 where
  header =
    "Rejected at ?"
      <> Text.pack (show $ interactionId $ refusalGoalId refusal)
      <> renderBatchPosition (refusalPosition refusal)
      <> ". No edits were written."
  (reason, warnings) = case refusalReason refusal of
    RefusedUnknownGoal ->
      ( indent
          "There is no such goal in the current load. Check the goal IDs in \
          \the most recent result."
      , []
      )
    RefusedError e ->
      (indent $ errorMessage e, map renderWarning $ errorWarnings e)

renderBatchPosition :: BatchPosition -> Text
renderBatchPosition position
  | batchLength position == 1 = ""
  | otherwise =
      " (item "
        <> Text.pack (show $ batchIndex position + 1)
        <> " of "
        <> Text.pack (show $ batchLength position)
        <> ")"
