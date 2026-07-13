{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- End-to-end test suite. Deliberately thin compared to the integration test
-- suite. Purpose is to check things like that stdout only contains parseable
-- JSON-RPC lines, etc.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Aeson (FromJSON, Value, eitherDecodeStrict, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Directory (copyFile, doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (
  CreateProcess (..),
  StdStream (CreatePipe),
  createProcess,
  proc,
  waitForProcess,
 )
import System.Timeout (timeout)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import JSONRPC (
  JSONRPCError (..),
  JSONRPCErrorInfo (..),
  JSONRPCMessage (..),
  JSONRPCResponse (..),
  RequestId (RequestId),
  toJSONRPCNotification,
  toJSONRPCRequest,
 )
import MCP.Protocol (
  CallToolParams (..),
  CallToolRequest (..),
  CallToolResult (..),
  InitializeParams (..),
  InitializeRequest (..),
  InitializeResult (..),
  InitializedNotification (..),
  ListToolsRequest (..),
  ListToolsResult (..),
 )
import MCP.Types (
  ClientCapabilities (..),
  ContentBlock (..),
  Implementation (..),
  TextContent (..),
  Tool (..),
 )

main :: IO ()
main =
  defaultMain $
    testGroup
      "end-to-end"
      [ handshakeTest
      , loadTest
      , giveTest
      , argumentErrorTest
      ]

handshakeTest :: TestTree
handshakeTest =
  testCase "initialize and tools/list handshake" $ do
    responses <-
      serverExchange
        [ toJSON (toJSONRPCRequest initializeRequest)
        , toJSON (toJSONRPCNotification initializedNotification)
        , toJSON (toJSONRPCRequest (listToolsRequest 2))
        ]
    initializeResult <- resultFor 1 responses :: IO InitializeResult
    initializeResult.serverInfo.name @?= "agda-mcp"
    toolsResult <- resultFor 2 responses :: IO ListToolsResult
    let names = map (.name) toolsResult.tools
    assertBool "tools/list carries load and give" $
      "load" `elem` names && "give" `elem` names

loadTest :: TestTree
loadTest =
  testCase "load reports the goal" $
    withFixture "Hole.agda" $ \path -> do
      responses <-
        serverExchange
          [ toJSON (toJSONRPCRequest initializeRequest)
          , toJSON (toJSONRPCNotification initializedNotification)
          , toJSON (toJSONRPCRequest (callToolRequest 2 "load" [("path", toJSON path)]))
          ]
      result <- resultFor 2 responses :: IO CallToolResult
      result.isError @?= Just False
      text <- firstText result
      assertBool "reply begins with the load summary" $
        "Load succeeded: 1 goal." `Text.isPrefixOf` text

giveTest :: TestTree
giveTest =
  testCase "give edits the file on disk" $
    withFixture "Hole.agda" $ \path -> do
      original <- ByteString.readFile path
      responses <-
        serverExchange
          [ toJSON (toJSONRPCRequest initializeRequest)
          , toJSON (toJSONRPCNotification initializedNotification)
          , toJSON (toJSONRPCRequest (callToolRequest 2 "load" [("path", toJSON path)]))
          , toJSON
              ( toJSONRPCRequest
                  ( callToolRequest
                      3
                      "give"
                      [ ("path", toJSON path)
                      ,
                        ( "gives"
                        , toJSON [object ["goal" .= (0 :: Int), "expression" .= ("y" :: Text)]]
                        )
                      ]
                  )
              )
          ]
      result <- resultFor 3 responses :: IO CallToolResult
      result.isError @?= Just False
      after <- ByteString.readFile path
      after @?= encodeUtf8 (Text.replace "{!!}" "y" (decodeUtf8 original))

argumentErrorTest :: TestTree
argumentErrorTest =
  testCase "malformed arguments error without killing the server" $
    withFixture "Hole.agda" $ \path -> do
      responses <-
        serverExchange
          [ toJSON (toJSONRPCRequest initializeRequest)
          , toJSON (toJSONRPCNotification initializedNotification)
          , toJSON
              ( toJSONRPCRequest
                  ( callToolRequest
                      2
                      "give"
                      [ ("path", toJSON (42 :: Int)) -- wrong-typed argument
                      ,
                        ( "gives"
                        , toJSON [object ["goal" .= (0 :: Int), "expression" .= ("y" :: Text)]]
                        )
                      ]
                  )
              )
          , toJSON (toJSONRPCRequest (callToolRequest 3 "load" [("path", toJSON path)]))
          ]
      giveResult <- resultFor 2 responses :: IO CallToolResult
      giveResult.isError @?= Just True
      -- A parse error for a tool doesn't take the server down
      loadResult <- resultFor 3 responses :: IO CallToolResult
      loadResult.isError @?= Just False

-- JSON RPC request builders

initializeRequest :: InitializeRequest
initializeRequest =
  InitializeRequest
    (RequestId (toJSON (1 :: Int)))
    ( InitializeParams
        "2025-06-18"
        (ClientCapabilities Nothing Nothing Nothing Nothing)
        (Implementation "agda-mcp-end-to-end" "0" Nothing)
    )

initializedNotification :: InitializedNotification
initializedNotification = InitializedNotification Nothing

listToolsRequest :: Int -> ListToolsRequest
listToolsRequest n = ListToolsRequest (RequestId (toJSON n)) Nothing

callToolRequest :: Int -> Text -> [(Text, Value)] -> CallToolRequest
callToolRequest n toolName arguments =
  CallToolRequest
    (RequestId (toJSON n))
    (CallToolParams toolName (Just (Map.fromList arguments)))

-- The first text block's text, as a tool result's `content` always carries
-- one for our tools.
firstText :: CallToolResult -> IO Text
firstText result = case result.content of
  (TextBlock textContent : _) -> pure textContent.text
  other -> assertFailure ("no text content block in " <> show other)

-- Look up the response or error message matching a request id among the
-- decoded messages, and decode its typed payload.
resultFor :: (FromJSON a) => Int -> [Value] -> IO a
resultFor n values = do
  messages <- traverse asMessage values
  case [m | m <- messages, messageId m == Just (RequestId (toJSON n))] of
    [ResponseMessage r] -> asResponse r.result
    [ErrorMessage e] ->
      assertFailure
        ( "JSON-RPC error "
            <> show e.error.code
            <> " for id "
            <> show n
            <> ": "
            <> Text.unpack e.error.message
        )
    other ->
      assertFailure
        ( "expected exactly one response for id "
            <> show n
            <> ", got "
            <> show (length other)
        )
 where
  asMessage :: Value -> IO JSONRPCMessage
  asMessage v = case Aeson.fromJSON v of
    Aeson.Success m -> pure m
    Aeson.Error e ->
      assertFailure
        ("stdout line is not a JSON-RPC message: " <> e <> ": " <> show v)

  asResponse :: (FromJSON a) => Value -> IO a
  asResponse v = case Aeson.fromJSON v of
    Aeson.Success a -> pure a
    Aeson.Error e ->
      assertFailure ("could not decode result for id " <> show n <> ": " <> e)

  messageId :: JSONRPCMessage -> Maybe RequestId
  messageId (ResponseMessage r) = Just r.id
  messageId (ErrorMessage e) = Just e.id
  messageId _ = Nothing

-- Locate the server binary, in the following order:
--
-- 1. an override AGDA_MCP_BIN environment variable
-- 2. on PATH (where `cabal test` puts it, via build-tool-depends)
-- 3. inside dist/build/agda-mcp/ (where nix's `./Setup test` leaves it, since
--    it ignores build-tool-depends)
findServerExecutable :: IO FilePath
findServerExecutable =
  runMaybeT
    ( MaybeT (lookupEnv "AGDA_MCP_BIN")
        <|> MaybeT (findExecutable "agda-mcp")
        <|> MaybeT setupBuildPath
    )
    >>= maybe
      ( assertFailure
          "agda-mcp binary not found: not in AGDA_MCP_BIN, not on \
          \PATH (build-tool-depends puts it there under cabal), and \
          \not in the Setup.hs dist/ build tree"
      )
      pure
 where
  setupBuildPath = do
    let path = "dist" </> "build" </> "agda-mcp" </> "agda-mcp"
    exists <- doesFileExist path
    pure (if exists then Just path else Nothing)

-- Pipe the requests into a fresh server process as line-delimited JSON-RPC,
-- close stdin, and return the decoded response lines. Asserts framing
-- invariants that any exchange should satisfy, namely exit 0 at stdin EOF,
-- empty stderr, and stdout carrying only parseable JSON lines.
serverExchange :: [Value] -> IO [Value]
serverExchange requests = do
  serverPath <- findServerExecutable
  -- 120 s, `timeout` takes microseconds
  result <- timeout (120 * 10 ^ 6) $ do
    (Just stdinHandle, Just stdoutHandle, Just stderrHandle, process) <-
      createProcess
        (proc serverPath [])
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
    ByteString.hPut stdinHandle (encodeLines requests)
    hClose stdinHandle
    out <- ByteString.hGetContents stdoutHandle
    err <- ByteString.hGetContents stderrHandle
    code <- waitForProcess process
    pure (code, out, err)
  (code, out, err) <-
    maybe (assertFailure "server exchange timed out") pure result
  code @?= ExitSuccess
  err @?= ByteString.empty
  traverse decodeLine (Char8.lines out)
 where
  encodeLines =
    ByteString.concat . map ((<> "\n") . LazyByteString.toStrict . Aeson.encode)

  decodeLine line =
    either
      (\e -> assertFailure ("stdout line is not JSON: " <> e <> ": " <> show line))
      pure
      (eitherDecodeStrict line :: Either String Value)

withFixture :: FilePath -> (FilePath -> IO a) -> IO a
withFixture name action =
  withSystemTempDirectory "agda-mcp-end-to-end" $ \directory -> do
    let target = directory </> name
    copyFile ("test" </> "fixtures" </> name) target
    action target
