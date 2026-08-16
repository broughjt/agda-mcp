{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.MCP (
  Tool (..),
  Budget (..),
  inspectBudget,
  loadBudget,
  editBudget,
  textToolHandle,
  parseArguments,
  parseTimeout,
  parseNormalizationField,
  normalizations,
  renderNormalization,
  loadIdSchema,
  goalIdSchema,
  normalizationSchema,
  timeoutSchema,
) where

import Agda.Interaction.Base (Rewrite (..))
import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON (..), Value)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  Content (..),
  Error,
  Schema,
  SchemaType (..),
  ToolDefinition,
  ToolResult,
  describedSchema,
  toolError,
  toolResult,
 )

import AgdaMCP.Tools.Render (renderEditingTimeout, renderReadOnlyTimeout)
import AgdaMCP.Tools.State (ToolM, ToolState, runToolM, withBudget)

{- | A tool definition together with the handler that runs it. The library
pairs the two only through the call handler's name dispatch, so we carry them
side by side and let `AgdaMCP.handlers` do the lookup.
-}
data Tool = Tool
  { toolDefinition :: ToolDefinition
  , toolRun :: MVar ToolState -> Map Text Value -> IO (Either Error ToolResult)
  }

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

{- | Build a tool handler that decodes its arguments, runs the tool under its
timeout, and reports the rendered response as text.

Arguments that fail to decode, and calls that run out of time, are reported as
a tool result with `isError` set rather than as a JSON-RPC error, so the model
sees the problem and can correct it.

The timeout is handled here rather than by the tool, so it never reaches the
request types and the tools stay unaware of it.
-}
textToolHandle ::
  (FromJSON p) =>
  Budget ->
  (p -> ToolM q) ->
  (q -> Either Text Text) ->
  (MVar ToolState -> Map Text Value -> IO (Either Error ToolResult))
textToolHandle budget handle renderResponse stateVar arguments =
  fmap Right $
    case (,) <$> parseArguments arguments <*> parseTimeout budget arguments of
      Left message -> pure $ toolError message
      Right (request, seconds) -> do
        result <-
          runToolM stateVar $
            withBudget (seconds * microsecondsPerSecond) (handle request)
        pure $ maybe (toolError $ budgetTimedOut budget seconds) rendered result
 where
  rendered =
    either toolError (toolResult . (: []) . ContentText) . renderResponse

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

normalizations :: Map Text Rewrite
normalizations =
  Map.fromList
    [ ("asis", AsIs)
    , ("instantiated", Instantiated)
    , ("headnormal", HeadNormal)
    , ("simplified", Simplified)
    , ("normalized", Normalised)
    ]

renderNormalization :: Rewrite -> Text
renderNormalization AsIs = "asis"
renderNormalization Instantiated = "instantiated"
renderNormalization HeadNormal = "headnormal"
renderNormalization Simplified = "simplified"
renderNormalization Normalised = "normalized"

-- Schema elements shared by multiple tools

loadIdSchema :: Schema
loadIdSchema = describedSchema description $ SchemaString Nothing
 where
  description :: Text
  description =
    "The load ID from the load result that issued this goal ID. Each load makes fresh goal ID assignments, so use the most recently issued load ID and goal IDs. If you no longer have that result, load the file again."

goalIdSchema :: Schema
goalIdSchema = describedSchema description SchemaInteger
 where
  description :: Text
  description = "The target goal's interaction ID (the N in `?N`)."

timeoutSchema :: Budget -> Schema
timeoutSchema budget = describedSchema description SchemaInteger
 where
  description :: Text
  description =
    "How long this call may run, in seconds, before the server cancels it. Defaults to "
      <> Text.pack (show $ budgetDefault budget)
      <> "."

normalizationSchema :: Schema
normalizationSchema =
  describedSchema description $ SchemaString (Just $ Map.keys normalizations)
 where
  description :: Text
  description =
    "How much to normalize reported types. Defaults to `simplified`, which is what Agda's interactive goal display shows, but which does not unfold definitions. `normalized` unfolds definitions all the way, while `asis` reports types exactly as written."
