{-# LANGUAGE OverloadedStrings #-}

module Test.Tool (tests) where

import Test.Tasty (TestTree, testGroup, withResource)

import Test.Harness (warmInteractionState)
import Test.Tool.CaseSplit qualified as CaseSplit
import Test.Tool.Check qualified as Check
import Test.Tool.Give qualified as Give
import Test.Tool.Goal qualified as Goal
import Test.Tool.Load qualified as Load
import Test.Tool.Scenario qualified as Scenario
import Test.Tool.Source qualified as Source

tests :: TestTree
tests =
  withResource warmInteractionState (const $ pure ()) $ \warm ->
    testGroup
      "Tool"
      [ Source.tests
      , Load.tests
      , Goal.tests warm
      , Check.tests warm
      , Give.tests warm
      , CaseSplit.tests warm
      , Scenario.tests warm
      ]
