{-# LANGUAGE OverloadedStrings #-}

module Test.Model (tests) where

import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AgdaMCP.Interaction.Model (
  Position (..),
  Span (..),
  spanLength,
  spanText,
 )

tests :: TestTree
tests =
  testGroup
    "Model"
    [ spanLengthTests
    , spanTextTests
    ]

spanLengthTests :: TestTree
spanLengthTests =
  testGroup
    "spanLength"
    [ testCase "a hole is one position wide" $
        spanLength hole @?= 1
    , testCase "a multiline span counts the line break" $
        spanLength typeThroughName @?= 3
    , testCase "an empty span is zero wide" $
        spanLength (Span (Position 10 2 5) (Position 10 2 5)) @?= 0
    ]

spanTextTests :: TestTree
spanTextTests =
  testGroup
    "spanText"
    [ testCase "extracts the text under a hole" $
        spanText source hole @?= "?"
    , testCase "offsets count code points, not bytes" $
        spanText source naturals @?= "ℕ"
    , testCase "a multiline span includes the line break" $
        spanText source typeThroughName @?= "ℕ\nf"
    , testCase "an empty span extracts nothing" $
        spanText source (Span (Position 10 2 5) (Position 10 2 5)) @?= ""
    ]

-- Offsets: f0 ' '1 :2 ' '3 ℕ4 \n5 f6 ' '7 =8 ' '9 ?10 \n11
source :: Text
source = "f : ℕ\nf = ?\n"

hole :: Span
hole = Span (Position 10 2 5) (Position 11 2 6)

naturals :: Span
naturals = Span (Position 4 1 5) (Position 5 1 6)

typeThroughName :: Span
typeThroughName = Span (Position 4 1 5) (Position 7 2 2)
