{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.MCP (
  Tool (..),
  textToolHandle,
  parseArguments,
  parseNormalizationField,
  normalizations,
  renderNormalization,
  loadIdSchema,
  goalIdSchema,
  normalizationSchema,
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

import AgdaMCP.Tools.State (ToolM, ToolState, runToolM)

{- | A tool definition together with the handler that runs it. The library
pairs the two only through the call handler's name dispatch, so we carry them
side by side and let `AgdaMCP.handlers` do the lookup.
-}
data Tool = Tool
  { toolDefinition :: ToolDefinition
  , toolRun :: MVar ToolState -> Map Text Value -> IO (Either Error ToolResult)
  }

{- | Build a tool handler that decodes its arguments, runs the tool, and reports
the rendered response as text.

Arguments that fail to decode are reported as a tool result with `isError`
set rather than as a JSON-RPC error, so the model sees the problem and can
correct it.
-}
textToolHandle ::
  (FromJSON p) =>
  (p -> ToolM q) ->
  (q -> Either Text Text) ->
  (MVar ToolState -> Map Text Value -> IO (Either Error ToolResult))
textToolHandle handle renderResponse stateVar =
  either
    (pure . Right . toolError)
    ( fmap
        ( Right
            . either toolError (toolResult . (: []) . ContentText)
            . renderResponse
        )
        . runToolM stateVar
        . handle
    )
    . parseArguments

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

normalizationSchema :: Schema
normalizationSchema =
  describedSchema description $ SchemaString (Just $ Map.keys normalizations)
 where
  description :: Text
  description =
    "How much to normalize reported types. Defaults to `simplified`, which is what Agda's interactive goal display shows, but which does not unfold definitions. `normalized` unfolds definitions all the way, while `asis` reports types exactly as written."
