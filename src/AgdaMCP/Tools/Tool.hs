{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Tool (
  Tool (..),
  Answer (..),
  Budget (..),
  inspectBudget,
  loadBudget,
  editBudget,
  toolHandle,
  parseArguments,
  parseTimeout,
  parseNormalizationField,
  normalizations,
  renderNormalization,
) where

import Agda.Interaction.Base (Rewrite (..))
import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Tools.Description (Description)
import AgdaMCP.Tools.Json (kinded)
import AgdaMCP.Tools.Render (
  normalizations,
  renderEditingTimeout,
  renderNormalization,
  renderReadOnlyTimeout,
 )
import AgdaMCP.Tools.State (ToolM, ToolState, runToolM, withBudget)

{- | A tool: how it describes itself, protocol-neutrally, and the call that
runs it. Both front ends dispatch over the description's name and derive their
documentation from the rest of it.
-}
data Tool = Tool
  { toolDescription :: Description
  , toolCall :: MVar ToolState -> Map Text Value -> IO Answer
  }

{- | A tool result: whether the tool answered the question, the rendered prose,
and the same answer as JSON.
-}
data Answer = Answer
  { answerOk :: Bool
  , answerText :: Text
  , answerValue :: Value
  }
  deriving (Eq, Show)

{- | How long a tool's calls may run before the server cancels them, and how to
report this.
-}
data Budget = Budget
  { budgetDefault :: Int
  -- ^ Seconds, used by a call that does not supply its own `timeout`.
  , budgetTimedOut :: Int -> Text
  -- ^ Reports a call abandoned after the given number of seconds.
  }

inspectBudget, loadBudget, editBudget :: Budget
inspectBudget = Budget 30 renderReadOnlyTimeout
loadBudget = Budget 60 renderReadOnlyTimeout
editBudget = Budget 60 renderEditingTimeout

{- | Build a tool's call: decode its arguments, run it under its timeout, and
answer with the rendered response and its JSON. Arguments that fail to decode
answer @invalid_arguments@ and calls that run out of time answer @timed_out@,
each reported as a refusal rather than as a transport error, so the caller
sees the problem and can correct it.

The timeout is handled here rather than by the tool, so it never reaches the
request types and the tools stay unaware of it.
-}
toolHandle ::
  (FromJSON p, ToJSON q) =>
  Budget ->
  (p -> ToolM q) ->
  (q -> Either Text Text) ->
  (MVar ToolState -> Map Text Value -> IO Answer)
toolHandle budget handle renderResponse stateVar arguments =
  case (,) <$> parseArguments arguments <*> parseTimeout budget arguments of
    Left message ->
      pure $ refused message $ kinded "invalid_arguments" ["message" .= message]
    Right (request, seconds) -> do
      result <-
        runToolM stateVar $
          withBudget (seconds * microsecondsPerSecond) (handle request)
      pure $ case result of
        Nothing ->
          let message = budgetTimedOut budget seconds
           in refused message $
                kinded "timed_out" ["seconds" .= seconds, "message" .= message]
        Just response ->
          let rendered = renderResponse response
           in Answer
                { answerOk = isRight rendered
                , answerText = either id id rendered
                , answerValue = toJSON response
                }
 where
  refused message value =
    Answer {answerOk = False, answerText = message, answerValue = value}

-- Request parsing helpers

{- | Tool arguments arrive from the MCP layer as a map of top-level fields.
Rebuild the JSON object and decode it with the request type's `FromJSON`
instance.
-}
parseArguments :: (FromJSON a) => Map Text Value -> Either Text a
parseArguments =
  first Text.pack
    . Aeson.parseEither Aeson.parseJSON
    . Aeson.Object
    . KeyMap.fromMapText

{- | The call's timeout in seconds, defaulting to the tool's budget.

The value is clamped so that converting it to the microseconds `withBudget`
takes cannot overflow, but it is otherwise unbounded.
-}
parseTimeout :: Budget -> Map Text Value -> Either Text Int
parseTimeout budget arguments =
  case Map.lookup "timeout" arguments of
    Nothing -> Right $ budgetDefault budget
    Just value -> maybe (Left timeoutRefusal) Right $ seconds value
 where
  seconds :: Value -> Maybe Int
  seconds value = do
    -- Aeson's `Integer` refuses a fractional number and refuses an exponent
    -- large enough to be expensive to expand, both of which we want refused.
    requested <- either (const Nothing) Just $ Aeson.parseEither parseJSON value
    if requested > (0 :: Integer)
      then Just $ fromInteger $ min requested maximumSeconds
      else Nothing

  maximumSeconds :: Integer
  maximumSeconds =
    toInteger (maxBound :: Int) `div` toInteger microsecondsPerSecond

timeoutRefusal :: Text
timeoutRefusal =
  "expected timeout to be a whole number of seconds greater than zero"

microsecondsPerSecond :: Int
microsecondsPerSecond = 1000000

parseNormalization :: Value -> Aeson.Parser Rewrite
parseNormalization =
  Aeson.withText "normalization" $
    maybe
      ( fail $
          "expected one of "
            <> Text.unpack (Text.intercalate ", " $ Map.keys normalizations)
      )
      pure
      . flip Map.lookup normalizations

{- | Parse the requested normalization, defaulting to `Simplified`.

Agda's Emacs mode defaults to simplify for goal-display commands. Commands
like @Cmd_goal_type_context@ and friends are declared with
@agda2-maybe-normalised@ (agda2-mode.el:1228-1235), whose no-prefix-argument
level is `Simplified`.
-}
parseNormalizationField :: Aeson.Object -> Aeson.Parser Rewrite
parseNormalizationField o =
  fromMaybe Simplified
    <$> Aeson.explicitParseFieldMaybe parseNormalization o "normalization"
