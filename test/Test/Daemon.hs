module Test.Daemon (tests) where

import Test.Tasty (TestTree, testGroup, withResource)

import Test.Daemon.Key qualified as Key
import Test.Daemon.Protocol qualified as Protocol
import Test.Daemon.Server qualified as Server
import Test.Harness (warmInteractionState)

tests :: TestTree
tests =
  withResource warmInteractionState (const $ pure ()) $ \warm ->
    testGroup
      "Daemon"
      [ Key.tests
      , Protocol.tests
      , Server.tests warm
      ]
