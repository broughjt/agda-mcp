{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Protocol qualified as MCP
import MCP.Types qualified as MCPT
import System.IO (
    BufferMode (LineBuffering),
    hFlush,
    hIsEOF,
    hPutStrLn,
    hSetBuffering,
    hSetEncoding,
    stderr,
    stdin,
    stdout,
    utf8,
 )

main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    hPutStrLn stderr "agda-mcp fake MCP server listening on stdio"
    loop False

loop :: Bool -> IO ()
loop initialized = do
    eof <- hIsEOF stdin
    if eof
        then pure ()
        else do
            line <- BS.getLine
            initialized' <- handleLine initialized line
            loop initialized'

handleLine :: Bool -> BS.ByteString -> IO Bool
handleLine initialized line =
    case Aeson.eitherDecodeStrict' @MCP.JSONRPCMessage line of
        Left err -> do
            writeError (MCP.RequestId Aeson.Null) MCP.pARSE_ERROR (Text.pack err)
            pure initialized
        Right (MCP.RequestMessage req) -> handleRequest initialized req
        Right (MCP.NotificationMessage note) -> handleNotification initialized note
        Right (MCP.ResponseMessage _) -> pure initialized
        Right (MCP.ErrorMessage _) -> pure initialized

handleNotification :: Bool -> MCP.JSONRPCNotification -> IO Bool
handleNotification initialized (MCP.JSONRPCNotification _ methodName _) =
    case methodName of
        "notifications/initialized" -> pure initialized
        _ -> pure initialized

handleRequest :: Bool -> MCP.JSONRPCRequest -> IO Bool
handleRequest initialized req@(MCP.JSONRPCRequest version requestId methodName _requestParams)
    | version /= MCP.rPC_VERSION = do
        writeError requestId MCP.iNVALID_REQUEST "Invalid JSON-RPC version"
        pure initialized
    | not initialized && methodName `notElem` ["initialize", "ping"] = do
        writeError requestId MCP.sERVER_NOT_INITIALIZED "Server not initialized"
        pure initialized
    | otherwise =
        case methodName of
            "initialize" -> do
                writeResult req initializeResult
                pure True
            "ping" -> do
                writeResult req (MCPT.Result Nothing)
                pure initialized
            "tools/list" -> do
                writeResult req toolsResult
                pure initialized
            "tools/call" -> do
                handleToolCall req
                pure initialized
            unknownMethod -> do
                writeError requestId MCP.mETHOD_NOT_FOUND ("Unknown method: " <> unknownMethod)
                pure initialized

initializeResult :: MCP.InitializeResult
initializeResult =
    MCP.InitializeResult
        MCP.pROTOCOL_VERSION
        serverCapabilities
        (MCPT.Implementation "agda-mcp" "0.1.0.0" (Just "Agda MCP"))
        (Just "Experimental Agda MCP server stub. Currently exposes a fake hello tool.")
        Nothing

serverCapabilities :: MCPT.ServerCapabilities
serverCapabilities =
    MCPT.ServerCapabilities
        Nothing
        Nothing
        Nothing
        (Just (MCPT.ToolsCapability Nothing))
        Nothing
        Nothing

toolsResult :: MCP.ListToolsResult
toolsResult = MCP.ListToolsResult [helloTool] Nothing Nothing

helloTool :: MCPT.Tool
helloTool =
    MCPT.Tool
        "hello"
        (Just "Hello")
        (Just "Return a friendly greeting. This is a fake tool used to test MCP plumbing.")
        helloInputSchema
        Nothing
        (Just helloAnnotations)
        Nothing

helloInputSchema :: MCPT.InputSchema
helloInputSchema =
    MCPT.InputSchema
        "object"
        ( Just $
            Map.fromList
                [
                    ( "name"
                    , Aeson.object
                        [ "type" Aeson..= ("string" :: Text)
                        , "description" Aeson..= ("Name to greet." :: Text)
                        ]
                    )
                ]
        )
        Nothing

helloAnnotations :: MCPT.ToolAnnotations
helloAnnotations =
    MCPT.ToolAnnotations
        (Just "Hello")
        (Just True)
        (Just False)
        (Just True)
        (Just False)

handleToolCall :: MCP.JSONRPCRequest -> IO ()
handleToolCall req@(MCP.JSONRPCRequest _ requestId _ requestParams) =
    case Aeson.fromJSON @MCP.CallToolParams requestParams of
        Aeson.Error err -> writeError requestId MCP.iNVALID_PARAMS (Text.pack err)
        Aeson.Success (MCP.CallToolParams toolName arguments) ->
            case toolName of
                "hello" -> writeResult req (helloResult (extractName arguments))
                unknownTool -> writeError requestId MCP.mETHOD_NOT_FOUND ("Unknown tool: " <> unknownTool)

helloResult :: Text -> MCP.CallToolResult
helloResult who =
    MCP.CallToolResult
        [MCPT.TextBlock (MCPT.TextContent "text" ("Hello, " <> who <> "!") Nothing Nothing)]
        (Just (Map.fromList [("greeting", Aeson.String ("Hello, " <> who <> "!"))]))
        (Just False)
        Nothing

extractName :: Maybe (Map Text Aeson.Value) -> Text
extractName maybeArgs =
    case maybeArgs >>= Map.lookup "name" of
        Just (Aeson.String name) | not (Text.null name) -> name
        _ -> "world"

writeResult :: (Aeson.ToJSON result) => MCP.JSONRPCRequest -> result -> IO ()
writeResult (MCP.JSONRPCRequest _ requestId _ _) result =
    writeMessage $
        MCP.ResponseMessage $
            MCP.JSONRPCResponse MCP.rPC_VERSION requestId (Aeson.toJSON result)

writeError :: MCP.RequestId -> Int -> Text -> IO ()
writeError requestId code message =
    writeMessage $
        MCP.ErrorMessage $
            MCP.JSONRPCError
                MCP.rPC_VERSION
                requestId
                (MCP.JSONRPCErrorInfo code message Nothing)

writeMessage :: MCP.JSONRPCMessage -> IO ()
writeMessage message = do
    LBS.putStrLn (Aeson.encode message)
    hFlush stdout
