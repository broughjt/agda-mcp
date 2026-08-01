{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.MCP (
  textToolHandle,
  parseArguments,
  parseNormalizationField,
  normalizations,
  loadIdSchema,
  goalIdSchema,
  normalizationSchema,
) where

import Agda.Interaction.Base (Rewrite (..))
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
  CallToolResult,
  MCPServerT,
  ProcessResult (..),
  toolTextError,
  toolTextResult,
 )

import AgdaMCP.Tools.State (ToolM, runToolM)

textToolHandle ::
  (FromJSON p) =>
  (p -> ToolM q) ->
  (q -> Text) ->
  (Maybe (Map Text Value) -> MCPServerT (ProcessResult CallToolResult))
textToolHandle handle renderResponse =
  either
    (pure . ProcessSuccess . toolTextError)
    ( fmap (ProcessSuccess . toolTextResult . (: []) . renderResponse)
        . runToolM
        . handle
    )
    . parseArguments

-- Request parsing helpers

-- Tool arguments arrive from the MCP layer as a map of top-level fields.
-- Rebuild the JSON object and decode it with the request type's `FromJSON`
-- instance.
parseArguments :: (FromJSON a) => Maybe (Map Text Value) -> Either Text a
parseArguments =
  first Text.pack
    . Aeson.parseEither Aeson.parseJSON
    . Aeson.Object
    . KeyMap.fromMapText
    . fromMaybe Map.empty

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

-- The requested normalization, defaulting the way Agda's own Emacs mode
-- defaults the goal-display commands: `Cmd_goal_type_context` and friends are
-- declared with `agda2-maybe-normalised` (agda2-mode.el:1228-1235), whose
-- no-prefix-argument level is `Simplified`.
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

-- Schema elements shared by multiple tools

loadIdSchema :: Value
loadIdSchema =
  Aeson.object
    [ "type" Aeson..= ("string" :: Text)
    , "description"
        -- TODO:
        Aeson..= ( "The `load_id` from the load result that issued this goal \
                   \ID. Goal IDs are renumbered by every load, so an ID from an \
                   \earlier load is refused rather than misread." ::
                     Text
                 )
    ]

goalIdSchema :: Value
goalIdSchema =
  Aeson.object
    [ "type" Aeson..= ("integer" :: Text)
    , "description"
        -- TODO:
        Aeson..= ("The target goal's interaction ID (`?N`) from a load result" :: Text)
    ]

normalizationSchema :: Value
normalizationSchema =
  Aeson.object
    [ "type" Aeson..= ("string" :: Text)
    , "enum" Aeson..= (Map.keys normalizations :: [Text])
    , "description"
        -- TODO:
        Aeson..= ( "How much to normalize the reported goal type and context. \
                   \Defaults to `simplified`, which is what Agda's own goal \
                   \display shows; `normalized` unfolds definitions all the \
                   \way, `asis` reports the types exactly as written." ::
                     Text
                 )
    ]
