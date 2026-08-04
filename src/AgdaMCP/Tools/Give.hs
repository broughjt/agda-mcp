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

import Agda.Syntax.Common (InteractionId (..))
import Data.Aeson (FromJSON (..), object, (.:), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction (Error (..), Span)
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadId, LoadIdRefusal, renderLoadIdRefusal)
import AgdaMCP.Tools.MCP (goalIdSchema, loadIdSchema, textToolHandle)
import AgdaMCP.Tools.Render (blocks, indent, renderWarning, section)
import AgdaMCP.Tools.State (ToolM)

giveTool :: ToolHandler
giveTool =
  toolHandler
    "give"
    ( Just
        "Fill open goals in the currently loaded Agda file by writing \
        \expressions into them. Takes a `load_id` and a non-empty list of \
        \`gives`, each a goal interaction ID, an expression, and an optional \
        \`action`: `give` (default) checks the expression against the goal and \
        \writes it, `refine` writes a partial term and leaves new goals \
        \behind. The batch is all-or-nothing and checked in order: if one item \
        \is rejected, the rest are skipped and no edits are written. Before \
        \writing, the server checks that the file on disk still matches the \
        \source Agda checked; if it changed, everything is refused. Every \
        \outcome is followed by a reload that issues a fresh `load_id` and \
        \renumbers goals, so use the goal IDs from the result below rather \
        \than the ones you sent. What gets written is Agda's own elaborated \
        \expression, which may differ from your text — the result shows what \
        \went in. Files are written back as UTF-8 with LF line endings."
    )
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
                    , "description"
                        .= ( "An all-or-nothing batch of goals to fill, applied \
                             \in order. Each goal may appear at most once." ::
                               Text
                           )
                    , "items"
                        .= object
                          [ "type" .= ("object" :: Text)
                          , "properties"
                              .= object
                                [ "goal" .= goalIdSchema
                                , "expression"
                                    .= object
                                      [ "type" .= ("string" :: Text)
                                      , "description"
                                          .= ( "The Agda expression to write \
                                               \into the goal." ::
                                                 Text
                                             )
                                      ]
                                , "action"
                                    .= object
                                      [ "type" .= ("string" :: Text)
                                      , "enum" .= (Map.keys actions :: [Text])
                                      , "default" .= ("give" :: Text)
                                      , "description"
                                          .= ( "`give` checks the expression \
                                               \against the goal and fills it; \
                                               \`refine` writes a partial term, \
                                               \leaving new goals for the parts \
                                               \you left out. Defaults to \
                                               \`give`." ::
                                                 Text
                                             )
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
  = -- The load id was refused.
    ResponseRefused LoadIdRefusal
  | -- The outcome of the batch of actions, together with the result of a reload
    -- executed afterwards.
    ResponseCompleted Outcome Load.Response
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

-- Where in the batch the refusal happened. The index is zero-based.
data BatchPosition = BatchPosition {batchIndex :: Int, batchLength :: Int}
  deriving (Eq, Show)

data RefusalReason
  = RefusedUnknownGoal
  | RefusedError Error
  deriving (Eq, Show)

-- Business logic

give :: Request -> ToolM Response
give = error "un"

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

-- The batch is all-or-nothing and applied in order, so an empty batch has
-- nothing to do and a repeated goal id would ask us to fill a hole that the
-- earlier item already solved.
parseItems :: Aeson.Value -> Aeson.Parser [Item]
parseItems = Aeson.withArray "gives" $ \values -> do
  items <-
    traverse
      (\(index, value) -> parseItem value Aeson.<?> Aeson.Index index)
      (zip [0 ..] $ toList values)
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
renderOutcome OutcomeFileChanged =
  "The file on disk no longer matches the source Agda checked, so no edits \
  \were written. It has been reloaded below; goal IDs from the earlier load \
  \are no longer valid."
renderOutcome (OutcomeSourceUnreadable message) =
  blocks
    [ "Could not read the file to check it still matches the source Agda \
      \checked, so no edits were written."
    , indent message
    ]
renderOutcome (OutcomeWriteFailed message) =
  blocks
    [ "Could not write the file. It was not modified."
    , indent message
    ]

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
          \the most recent load result."
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
