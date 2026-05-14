{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools
    ( agdaTools
    , agdaLoadTool
    , agdaGiveTool
    ) where

import AgdaMCP.Runtime (AgdaRuntime, agdaGive, agdaLoad)
import AgdaMCP.Types (AgdaError, renderAgdaError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server qualified as MCP

-- | MCP tool definitions backed by the Agda runtime.
--
-- These are not wired into Main yet; Phase 1 just establishes the public shape.
agdaTools :: AgdaRuntime -> [MCP.ToolHandler]
agdaTools runtime =
    [ agdaLoadTool runtime
    , agdaGiveTool runtime
    ]

agdaLoadTool :: AgdaRuntime -> MCP.ToolHandler
agdaLoadTool runtime =
    MCP.toolHandler
        "agda_load"
        (Just "Load and type-check an Agda file, returning visible goals and diagnostics.")
        agdaLoadSchema
        $ \args -> case lookupText "file_path" args of
            Nothing -> pure $ MCP.ProcessSuccess $ MCP.toolTextError "Missing required argument: file_path"
            Just filePath -> do
                result <- liftIO $ agdaLoad runtime (textToFilePath filePath)
                pure $ either errorResult loadResult result

agdaGiveTool :: AgdaRuntime -> MCP.ToolHandler
agdaGiveTool runtime =
    MCP.toolHandler
        "agda_give"
        (Just "Fill a goal with an expression, apply the resulting source edit, then reload.")
        agdaGiveSchema
        $ \args -> case (lookupText "file_path" args, lookupInt "goal_id" args, lookupText "expression" args) of
            (Just filePath, Just goalId, Just expression) -> do
                result <- liftIO $ agdaGive runtime (textToFilePath filePath) goalId expression
                pure $ either errorResult giveResult result
            _ -> pure $ MCP.ProcessSuccess $ MCP.toolTextError "Missing required arguments: file_path, goal_id, expression"

agdaLoadSchema :: MCP.InputSchema
agdaLoadSchema =
    MCP.InputSchema
        "object"
        ( Just $
            Map.fromList
                [ ( "file_path"
                  , Aeson.object
                        [ "type" Aeson..= ("string" :: Text)
                        , "description" Aeson..= ("Path to the Agda file to load." :: Text)
                        ]
                  )
                ]
        )
        (Just ["file_path"])

agdaGiveSchema :: MCP.InputSchema
agdaGiveSchema =
    MCP.InputSchema
        "object"
        ( Just $
            Map.fromList
                [ ( "file_path"
                  , Aeson.object
                        [ "type" Aeson..= ("string" :: Text)
                        , "description" Aeson..= ("Path to the Agda file containing the goal." :: Text)
                        ]
                  )
                , ( "goal_id"
                  , Aeson.object
                        [ "type" Aeson..= ("integer" :: Text)
                        , "description" Aeson..= ("Agda interaction goal id." :: Text)
                        ]
                  )
                , ( "expression"
                  , Aeson.object
                        [ "type" Aeson..= ("string" :: Text)
                        , "description" Aeson..= ("Expression to give to the goal." :: Text)
                        ]
                  )
                ]
        )
        (Just ["file_path", "goal_id", "expression"])

loadResult :: Aeson.ToJSON result => result -> MCP.ProcessResult MCP.CallToolResult
loadResult result =
    MCP.ProcessSuccess $
        (MCP.toolTextResult ["agda_load completed"])
            { MCP.structuredContent = Just $ Map.fromList [("result", Aeson.toJSON result)]
            }

giveResult :: Aeson.ToJSON result => result -> MCP.ProcessResult MCP.CallToolResult
giveResult result =
    MCP.ProcessSuccess $
        (MCP.toolTextResult ["agda_give completed"])
            { MCP.structuredContent = Just $ Map.fromList [("result", Aeson.toJSON result)]
            }

errorResult :: AgdaError -> MCP.ProcessResult MCP.CallToolResult
errorResult err = MCP.ProcessSuccess $ MCP.toolTextError $ renderAgdaError err

lookupText :: Text -> Maybe (Map Text Aeson.Value) -> Maybe Text
lookupText key args = case args >>= Map.lookup key of
    Just (Aeson.String value) -> Just value
    _ -> Nothing

lookupInt :: Text -> Maybe (Map Text Aeson.Value) -> Maybe Int
lookupInt key args = case args >>= Map.lookup key of
    Just (Aeson.Number value) -> case Aeson.fromJSON (Aeson.Number value) of
        Aeson.Success intValue -> Just intValue
        Aeson.Error _ -> Nothing
    _ -> Nothing

textToFilePath :: Text -> FilePath
textToFilePath = Text.unpack
