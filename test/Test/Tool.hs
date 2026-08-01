{-# LANGUAGE OverloadedStrings #-}

module Test.Tool (tests) where

import Test.Tasty (TestTree, testGroup, withResource)

import Test.Harness (warmInteractionState)
import Test.Tool.Load qualified as Load
import Test.Tool.Scenario qualified as Scenario

tests :: TestTree
tests =
  testGroup
    "Tool"
    [ Load.tests
    , withResource warmInteractionState (const $ pure ()) Scenario.tests
    ]
