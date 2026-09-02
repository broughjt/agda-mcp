{-# LANGUAGE OverloadedStrings #-}

{- | The MCP-specific parts of a tool: the schema elements its definition is
built from.
-}
module AgdaMCP.Tools.MCP (
  loadIdSchema,
  goalIdSchema,
  normalizationSchema,
  timeoutSchema,
) where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  Schema,
  SchemaType (..),
  describedSchema,
 )

import AgdaMCP.Tools.Tool (Budget (..), normalizations)

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
