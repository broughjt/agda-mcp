{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.GoalTest (tests) where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Data.Aeson (Value, toJSON)

import Agda.Interaction.Base (Rewrite (Simplified))
import Agda.Syntax.Common (InteractionId (InteractionId))

import AgdaMCP.Position (Position (Position), Span (Span))
import AgdaMCP.Tools.Common (
  AgdaError (AgdaError),
  Warning (Warning),
  parseArguments,
 )
import AgdaMCP.Tools.Goal (
  GoalDetail (ExpressionGoal, PlainGoal),
  GoalDisplay (GoalDisplay),
  GoalRequest (GoalRequest),
  GoalResponse (GoalDisplayed, GoalFailed, GoalNotLoaded, GoalUnknown),
  GoalType (GoalType),
  renderGoalResponse,
 )
import AgdaMCP.Tools.Load (
  ContextEntry (ContextEntry),
  ContextEntryAttributes (ContextEntryAttributes),
  Goal (Goal),
  GoalShape (GoalOfType, GoalSort),
  LoadResponse (Loaded),
 )

tests :: TestTree
tests = testGroup "goal" [renderTests, parseArgumentTests]

renderTests :: TestTree
renderTests =
  testGroup
    "renderGoalResponse"
    [ testCase "plain goal with context, boundary, and constraints" $
        renderGoalResponse
          ( GoalDisplayed
              ( GoalDisplay
                  (InteractionId 0)
                  ( GoalType
                      (GoalOfType "double zero ≡ plus zero zero")
                      (Just (GoalOfType "zero ≡ zero"))
                  )
                  ( PlainGoal
                      [ ContextEntry "x" True "x" "Nat" Nothing True noAttributes
                      , ContextEntry
                          "one"
                          True
                          "one"
                          "Nat"
                          (Just "suc zero")
                          True
                          noAttributes
                      ]
                      ["i = i0 ⊢ zero"]
                      ["Check definition of Constraints.f : ?0 (blocked on _5)"]
                  )
              )
          )
          @?= "?0 : double zero ≡ plus zero zero\n\
              \normalized: zero ≡ zero\n\
              \  one : Nat\n\
              \  one = suc zero\n\
              \  x : Nat\n\n\
              \Boundary (wanted):\n\n\
              \i = i0 ⊢ zero\n\n\
              \Constraints on this goal:\n\n\
              \Check definition of Constraints.f : ?0 (blocked on _5)"
    , testCase "equal normalized rendering is suppressed" $
        renderGoalResponse
          ( GoalDisplayed
              ( GoalDisplay
                  (InteractionId 1)
                  (GoalType (GoalOfType "Nat") (Just (GoalOfType "Nat")))
                  (PlainGoal [] [] [])
              )
          )
          @?= "?1 : Nat"
    , testCase "requested normalization renders a single line" $
        renderGoalResponse
          ( GoalDisplayed
              ( GoalDisplay
                  (InteractionId 0)
                  (GoalType (GoalOfType "zero ≡ zero") Nothing)
                  (PlainGoal [] [] [])
              )
          )
          @?= "?0 : zero ≡ zero"
    , testCase "sort goal never shows a normalized line" $
        renderGoalResponse
          ( GoalDisplayed
              ( GoalDisplay
                  (InteractionId 3)
                  (GoalType GoalSort (Just GoalSort))
                  (PlainGoal [] [] [])
              )
          )
          @?= "Sort ?3"
    , testCase "expression goal with successful inference and checking" $
        renderGoalResponse
          ( GoalDisplayed
              ( GoalDisplay
                  (InteractionId 0)
                  (GoalType (GoalOfType "Nat") (Just (GoalOfType "Nat")))
                  (ExpressionGoal "y" (Right "Nat") (Right "y"))
              )
          )
          @?= "?0 : Nat\n\n\
              \Have: y : Nat\n\n\
              \Checks: elaborates to y"
    , testCase "failed infer renders with the relative-locations note" $
        renderGoalResponse
          ( GoalDisplayed
              ( GoalDisplay
                  (InteractionId 0)
                  (GoalType (GoalOfType "Nat → Nat") (Just (GoalOfType "Nat → Nat")))
                  ( ExpressionGoal
                      "λ ()"
                      (Left (AgdaError "1.1-5: error: [SomeError]\ndetail" Nothing []))
                      (Right "λ ()")
                  )
              )
          )
          @?= "?0 : Nat → Nat\n\n\
              \Infer failed (locations are relative to the submitted expression):\n\n\
              \1.1-5: error: [SomeError]\n\
              \detail\n\n\
              \Checks: elaborates to λ ()"
    , testCase "failed check renders its error and warnings" $
        renderGoalResponse
          ( GoalDisplayed
              ( GoalDisplay
                  (InteractionId 0)
                  ( GoalType
                      (GoalOfType "double zero ≡ plus zero zero")
                      (Just (GoalOfType "zero ≡ zero"))
                  )
                  ( ExpressionGoal
                      "zero"
                      (Right "Nat")
                      ( Left
                          ( AgdaError
                              "1.1-5: error: [UnequalTerms]\nNat !=< double zero ≡ plus zero zero"
                              Nothing
                              [Warning (Nothing, "a warning")]
                          )
                      )
                  )
              )
          )
          @?= "?0 : double zero ≡ plus zero zero\n\
              \normalized: zero ≡ zero\n\n\
              \Have: zero : Nat\n\n\
              \Check failed (locations are relative to the submitted expression):\n\n\
              \1.1-5: error: [UnequalTerms]\n\
              \Nat !=< double zero ≡ plus zero zero\n\n\
              \Warnings:\n\
              \a warning"
    , testCase "unknown goal explains the renumbering" $
        renderGoalResponse (GoalUnknown (InteractionId 9))
          @?= "No such goal ?9 in the loaded file. Goal IDs renumber after \
              \every edit or reload; use the IDs from the most recent load \
              \result."
    , testCase "failed query renders the error" $
        renderGoalResponse
          (GoalFailed (AgdaError "something environmental" Nothing []))
          @?= "The goal query failed:\n\n\
              \something environmental"
    , testCase "not-loaded refusal carries the fresh load result" $
        renderGoalResponse
          ( GoalNotLoaded
              ( Loaded
                  [ Goal
                      (InteractionId 0)
                      (Span (Position 0 8 12) (Position 4 8 16))
                      (GoalOfType "Nat")
                      []
                  ]
                  []
                  []
                  []
              )
          )
          @?= "Goal query refused: the file is not the currently loaded file, \
              \and goal interaction IDs are only valid for the most recently \
              \loaded file. Loaded the file; use the goal IDs from the fresh \
              \result below:\n\n\
              \Load succeeded: 1 goal.\n\n\
              \?0 : Nat (at 8:12-16)"
    ]

parseArgumentTests :: TestTree
parseArgumentTests =
  testGroup
    "parseArguments"
    [ testCase "valid full request" $
        case parseGoal
          [ ("path", "/tmp/Hole.agda")
          , ("goal", toJSON (1 :: Int))
          , ("normalization", "simplified")
          , ("expression", " y ")
          ] of
          Right (GoalRequest path goalId normalization expression) -> do
            path @?= "/tmp/Hole.agda"
            goalId @?= InteractionId 1
            normalization @?= Just Simplified
            expression @?= Just "y"
          Left message ->
            assertFailure ("unexpected parse failure: " <> Text.unpack message)
    , testCase "valid minimal request" $
        case parseGoal [("path", "/tmp/Hole.agda"), ("goal", toJSON (0 :: Int))] of
          Right (GoalRequest _ goalId normalization expression) -> do
            goalId @?= InteractionId 0
            normalization @?= Nothing
            expression @?= Nothing
          Left message ->
            assertFailure ("unexpected parse failure: " <> Text.unpack message)
    , testCase "missing goal" $
        expectParseFailure "goal" $
          parseGoal [("path", "/tmp/Hole.agda")]
    , testCase "string goal" $
        expectParseFailure "$.goal" $
          parseGoal [("path", "/tmp/Hole.agda"), ("goal", "0")]
    , testCase "non-integral goal" $
        expectParseFailure "$.goal" $
          parseGoal [("path", "/tmp/Hole.agda"), ("goal", toJSON (1.5 :: Double))]
    , testCase "unknown normalization level" $
        expectParseFailure "$.normalization" $
          parseGoal
            [ ("path", "/tmp/Hole.agda")
            , ("goal", toJSON (0 :: Int))
            , ("normalization", "full")
            ]
    , testCase "blank expression" $
        expectParseFailure "$.expression" $
          parseGoal
            [ ("path", "/tmp/Hole.agda")
            , ("goal", toJSON (0 :: Int))
            , ("expression", "   ")
            ]
    ]

parseGoal :: [(Text, Value)] -> Either Text GoalRequest
parseGoal = parseArguments . Just . Map.fromList

expectParseFailure :: Text -> Either Text GoalRequest -> IO ()
expectParseFailure fragment (Left message) =
  assertBool
    ( "expected the failure message to mention "
        <> Text.unpack fragment
        <> ", got: "
        <> Text.unpack message
    )
    (fragment `Text.isInfixOf` message)
expectParseFailure _ (Right _) =
  assertFailure "expected the parse to fail"

noAttributes :: ContextEntryAttributes
noAttributes = ContextEntryAttributes Nothing False Nothing Nothing False
