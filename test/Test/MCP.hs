{-# LANGUAGE OverloadedStrings #-}

module Test.MCP (tests) where

import Control.Concurrent.MVar (newEmptyMVar)
import Data.Aeson qualified as Aeson
import MCP.Server (ClientContext (..), McpServerInfo (..))
import MCP.Server.Handlers (handleToolsList)
import MCP.Server.JsonRpc (
  JsonRpcRequest (..),
  JsonRpcResponse (..),
  RequestId (..),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import AgdaMCP.MCP (handlers, serverInfo)
import AgdaMCP.Tools.Description (overview)

tests :: TestTree
tests =
  testGroup
    "MCP"
    [ testCase "tools/list answers the golden" $ do
        expected <-
          Aeson.eitherDecodeFileStrict "test/fixtures/tools-list.json"
            >>= either assertFailure pure
        stateVariable <- newEmptyMVar
        response <- handleToolsList (handlers stateVariable) context request
        responseError response @?= Nothing
        responseResult response @?= Just expected
    , testCase "the server's instructions are the overview" $
        serverInstructions serverInfo @?= overview
    ]
 where
  request =
    JsonRpcRequest
      { requestJsonrpc = "2.0"
      , requestId = RequestIdNumber 1
      , requestMethod = "tools/list"
      , requestParams = Nothing
      }
  context =
    ClientContext
      { clientToken = Nothing
      , clientPrincipal = Nothing
      , clientProtocolVersion = Nothing
      , clientInfo = Nothing
      , clientCapabilities = Nothing
      , reportProgress = \_ _ _ -> pure ()
      , logToClient = \_ _ -> pure ()
      }
