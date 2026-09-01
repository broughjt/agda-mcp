{-# LANGUAGE OverloadedStrings #-}

{- | The MCP-specific parts of a tool: the schema elements its definition is
built from, and the handler the MCP server runs.
-}
module AgdaMCP.Tools.MCP (
  textToolHandle,
  loadIdSchema,
  goalIdSchema,
  normalizationSchema,
  timeoutSchema,
) where

import Control.Concurrent.MVar (MVar)
import Data.Aeson (FromJSON (..), Value)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  Content (..),
  Error,
  Schema,
  SchemaType (..),
  ToolResult,
  describedSchema,
  toolError,
  toolResult,
 )

import AgdaMCP.Tools.State (ToolM, ToolState, runToolM, withBudget)
import AgdaMCP.Tools.Tool (
  Budget (..),
  normalizations,
  parseArguments,
  parseTimeout,
 )

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
            withBudget (seconds * 1000000) (handle request)
        pure $ maybe (toolError $ budgetTimedOut budget seconds) rendered result
 where
  rendered =
    either toolError (toolResult . (: []) . ContentText) . renderResponse

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
