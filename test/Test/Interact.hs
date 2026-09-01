module Test.Interact (tests) where

import Test.Tasty (TestTree, testGroup, withResource)

import Test.Harness (warmInteractionState)
import Test.Interact.CLI qualified as CLI
import Test.Interact.Daemon.Key qualified as Key
import Test.Interact.Daemon.Protocol qualified as Protocol
import Test.Interact.Daemon.Server qualified as Server
import Test.Interact.Help qualified as Help

tests :: TestTree
tests =
  withResource warmInteractionState (const $ pure ()) $ \warm ->
    testGroup
      "Interact"
      [ CLI.tests
      , Help.tests
      , testGroup
          "Daemon"
          [ Key.tests
          , Protocol.tests
          , Server.tests warm
          ]
      ]
