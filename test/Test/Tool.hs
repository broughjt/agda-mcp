{-# LANGUAGE OverloadedStrings #-}

module Test.Tool (tests) where

import Test.Tasty (TestTree, testGroup, withResource)

import Test.Harness (warmInteractionState)
import Test.Tool.Scenario qualified as Scenario

tests :: TestTree
tests =
  withResource warmInteractionState (const $ pure ()) $ \warm ->
    testGroup "Tool" (map ($ warm) [Scenario.tests])
