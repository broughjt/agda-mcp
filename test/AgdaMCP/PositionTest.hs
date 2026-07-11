{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.PositionTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AgdaMCP.Position (
  Position (Position),
  Span (Span),
  renderSpan,
 )

tests :: TestTree
tests =
  testGroup
    "renderSpan"
    [ testCase "same-line span omits the repeated end line" $
        renderSpan (Span (Position 0 20 1) (Position 14 20 15))
          @?= "20:1-15"
    , testCase "multiline span renders both complete endpoints" $
        renderSpan (Span (Position 0 20 1) (Position 30 21 15))
          @?= "20:1-21:15"
    ]
