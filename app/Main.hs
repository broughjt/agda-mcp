{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server qualified as MCP
import System.IO (
    BufferMode (LineBuffering),
    hPutStrLn,
    hSetBuffering,
    hSetEncoding,
    stderr,
    stdin,
    stdout,
    utf8,
 )

type instance MCP.MCPHandlerState = ()
type instance MCP.MCPHandlerUser = ()

main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetBuffering stderr LineBuffering
    hPutStrLn stderr "agda-mcp fake MCP server listening on stdio"
    MCP.serveStdio stdin stdout serverState

serverState :: MCP.MCPServerState
serverState =
    MCP.initMCPServerState
        ()
        Nothing
        Nothing
        serverCapabilities
        implementation
        instructions
        handlers

implementation :: MCP.Implementation
implementation =
    MCP.Implementation "agda-mcp" "0.1.0.0" (Just "Agda MCP")

instructions :: Maybe Text
instructions =
    Just "Experimental Agda MCP server stub. Currently exposes a fake hello tool."

serverCapabilities :: MCP.ServerCapabilities
serverCapabilities =
    MCP.ServerCapabilities
        Nothing
        Nothing
        Nothing
        (Just (MCP.ToolsCapability Nothing))
        Nothing
        Nothing

handlers :: MCP.ProcessHandlers
handlers = MCP.withToolHandlers [helloTool] MCP.defaultProcessHandlers

helloTool :: MCP.ToolHandler
helloTool =
    ( MCP.toolHandler
        "hello"
        (Just "Return a friendly greeting. This is a fake tool used to test MCP plumbing.")
        helloInputSchema
        $ \args -> pure $ MCP.ProcessSuccess $ helloResult $ extractName args
    )
        { MCP.tool_title = Just "Hello"
        , MCP.tool_annotations = Just helloAnnotations
        }

helloInputSchema :: MCP.InputSchema
helloInputSchema =
    MCP.InputSchema
        "object"
        ( Just $
            Map.fromList
                [ ( "name"
                  , Aeson.object
                        [ "type" Aeson..= ("string" :: Text)
                        , "description" Aeson..= ("Name to greet." :: Text)
                        ]
                  )
                ]
        )
        Nothing

helloAnnotations :: MCP.ToolAnnotations
helloAnnotations =
    MCP.ToolAnnotations
        (Just "Hello")
        (Just True)
        (Just False)
        (Just True)
        (Just False)

helloResult :: Text -> MCP.CallToolResult
helloResult who =
    MCP.CallToolResult
        [MCP.TextBlock (MCP.TextContent "text" greeting Nothing Nothing)]
        (Just (Map.fromList [("greeting", Aeson.String greeting)]))
        (Just False)
        Nothing
  where
    greeting = "Hello, " <> who <> "!"

extractName :: Maybe (Map Text Aeson.Value) -> Text
extractName maybeArgs =
    case maybeArgs >>= Map.lookup "name" of
        Just (Aeson.String name) | not (Text.null name) -> name
        _ -> "world"
