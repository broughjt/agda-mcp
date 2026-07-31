{-# LANGUAGE OverloadedStrings #-}

module Test.Tool (tests) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Tool"
    []
