{-# LANGUAGE OverloadedStrings #-}

module Test.Interaction (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import AgdaMCP.Interaction.Load (Request (..), Response (..), load)
import AgdaMCP.Interaction.Model (Goal (..), GoalShape (..))
import Test.Harness (withFixtureSession)

tests :: TestTree
tests =
  testGroup
    "interaction"
    [ testGroup
        "Load"
        [ testCase "load a file with single hole of type ℕ" $ do
            response <-
              withFixtureSession "test/fixtures/HoleNatural.agda" $ \path ->
                load Request {requestPath = path, requestArguments = []}
            case response of
              ResponseOk goals hiddenMetavariables warnings nonFatalErrors -> do
                map (\goal -> (goalId goal, goalShape goal)) goals @?= [(0, GoalOfType "ℕ")]
                hiddenMetavariables @?= []
                warnings @?= []
                nonFatalErrors @?= []
              other ->
                assertFailure $ "expected ResponseOk, got " <> show other
        , testCase "load a group-theory file with three holes" $ do
            response <-
              withFixtureSession "test/fixtures/GroupProperties.agda" $ \path ->
                load Request {requestPath = path, requestArguments = []}
            case response of
              ResponseOk goals hiddenMetavariables warnings nonFatalErrors -> do
                map goalId goals @?= [0, 1, 2]
                map goalShape goals
                  @?= [ GoalOfType "x ≈ y → u ≈ v → x // u ≈ y // v"
                      , GoalOfType "(setoid Relation.Binary.Bundles.Setoid.≈ (y ∙ ε)) y"
                      , GoalOfType "ε ⁻¹ ≈ ε"
                      ]
                hiddenMetavariables @?= []
                warnings @?= []
                nonFatalErrors @?= []
              other ->
                assertFailure $ "expected ResponseOk, got " <> show other
        ]
    ]
