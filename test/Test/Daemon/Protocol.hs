{-# LANGUAGE OverloadedStrings #-}

module Test.Daemon.Protocol (tests) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Time (UTCTime (..), fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import AgdaMCP.Daemon.Protocol (
  BuildIdentity (..),
  Control (..),
  Hello (..),
  HelloReply (..),
  Info (..),
  Request (..),
  Response (..),
 )

tests :: TestTree
tests =
  testGroup
    "Protocol"
    [ testCase "a build identity is an object of its three fields" $
        encodesTo
          build
          ( Aeson.object
              [ "executable" Aeson..= ("/nix/store/abc/bin/agda-mcp" :: String)
              , "size" Aeson..= (123456 :: Integer)
              , "modified" Aeson..= (1 :: Integer)
              ]
          )
    , testCase "a hello carries the build" $
        encodesTo
          (HelloBuild build)
          (Aeson.object ["build" Aeson..= Aeson.toJSON build])
    , testCase "a stop control" $
        encodesTo
          (HelloControl ControlStop)
          (Aeson.object ["control" Aeson..= ("stop" :: String)])
    , testCase "a status control" $
        encodesTo
          (HelloControl ControlStatus)
          (Aeson.object ["control" Aeson..= ("status" :: String)])
    , testCase "an accepted hello" $
        encodesTo BuildOk (Aeson.object ["build" Aeson..= ("ok" :: String)])
    , testCase "a mismatch names the daemon's build" $
        encodesTo
          (BuildMismatch build)
          ( Aeson.object
              [ "build" Aeson..= ("mismatch" :: String)
              , "daemon" Aeson..= Aeson.toJSON build
              ]
          )
    , testCase "a request is the tool name and its arguments" $
        encodesTo
          ( Request
              "goal"
              ( Map.fromList
                  [("load_id", Aeson.String "L1-k7x2"), ("goal", Aeson.Number 0)]
              )
          )
          ( Aeson.object
              [ "tool" Aeson..= ("goal" :: String)
              , "arguments"
                  Aeson..= Aeson.object
                    [ "load_id" Aeson..= ("L1-k7x2" :: String)
                    , "goal" Aeson..= (0 :: Int)
                    ]
              ]
          )
    , testCase "a response is ok and text" $
        encodesTo
          (Response False "No load is current.")
          ( Aeson.object
              [ "ok" Aeson..= False
              , "text" Aeson..= ("No load is current." :: String)
              ]
          )
    , testCase "the info file round-trips" $
        roundTrips
          Info
            { infoRoot = "/work/proj"
            , infoFlags = ["--include-path", "src"]
            , infoPid = 4242
            , infoBuild = build
            , infoAgdaVersion = "2.8.0"
            , infoStarted = UTCTime (fromGregorian 2026 9 1) 0
            }
    ]

build :: BuildIdentity
build =
  BuildIdentity
    { buildExecutable = "/nix/store/abc/bin/agda-mcp"
    , buildSize = 123456
    , buildModified = 1
    }

-- | The value encodes to exactly this JSON, and decodes back from it.
encodesTo :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> Value -> Assertion
encodesTo value expected = do
  Aeson.toJSON value @?= expected
  Aeson.fromJSON expected @?= Aeson.Success value

roundTrips :: (ToJSON a, FromJSON a, Eq a, Show a) => a -> Assertion
roundTrips value = Aeson.decode (Aeson.encode value) @?= Just value
