{-# LANGUAGE OverloadedStrings #-}

module Test.Interaction (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Agda.Interaction.Base (Rewrite (..))

import AgdaMCP.Interaction.Load (Request (..), Response (..), load)
import AgdaMCP.Interaction.Model (
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  NonFatalError (..),
 )
import Data.Functor (void)
import Test.Harness (
  currentFile,
  expectLoadError,
  expectLoaded,
  withFixtureSession,
 )

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
        , testCase "load a file with three holes" $ do
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
        , testCase "a successful load records the current file" $ do
            (path, recorded) <-
              withFixtureSession "test/fixtures/HoleNatural.agda" $ \path -> do
                response <- load Request {requestPath = path, requestArguments = []}
                void $ liftIO $ expectLoaded "load" response
                (,) path <$> currentFile
            recorded @?= Just path
        , testCase "a failed load clears the recorded current file" $ do
            (afterSuccess, afterFailure) <-
              withFixtureSession "test/fixtures/HoleNatural.agda" $ \path -> do
                let request = Request {requestPath = path, requestArguments = []}
                void $ load request >>= liftIO . expectLoaded "initial load"
                recorded <- currentFile
                liftIO $ Text.IO.writeFile path brokenHoleNatural
                void $ load request >>= liftIO . expectLoadError "load after breaking"
                (,) recorded <$> currentFile
            assertBool "the first load should have recorded a file" (isJust afterSuccess)
            afterFailure @?= Nothing
        , testCase
            "when loading, causing a failure and loading, and then restoring \
            \and loading, the first and last responses should be equal"
            $ do
              (before, after) <-
                withFixtureSession "test/fixtures/HoleNatural.agda" $ \path -> do
                  let request = Request {requestPath = path, requestArguments = []}
                  original <- liftIO $ Text.IO.readFile path
                  before <- load request
                  void $ liftIO $ expectLoaded "initial load" before
                  liftIO $ Text.IO.writeFile path brokenHoleNatural
                  void $ load request >>= liftIO . expectLoadError "load after breaking"
                  liftIO $ Text.IO.writeFile path original
                  after <- load request
                  pure (before, after)
              after @?= before
        , testCase "load arguments do not persist across loads" $ do
            (withSafe, withoutSafe) <-
              withFixtureSession "test/fixtures/SafePostulate.agda" $ \path -> do
                withSafe <-
                  load Request {requestPath = path, requestArguments = ["--safe"]}
                withoutSafe <-
                  load Request {requestPath = path, requestArguments = []}
                pure (withSafe, withoutSafe)
            -- `--safe` rejects the postulate as a non-fatal error
            (goals, hiddenMetavariables, warnings, nonFatalErrors) <-
              expectLoaded "load with --safe" withSafe
            goals @?= []
            hiddenMetavariables @?= []
            warnings @?= []
            case nonFatalErrors of
              [NonFatalError (_, message)] ->
                assertBool
                  ("expected a SafeFlagPostulate error, got " <> Text.unpack message)
                  ("SafeFlagPostulate" `Text.isInfixOf` message)
              other ->
                assertFailure $ "expected one non-fatal error, got " <> show other
            expectLoaded "load without arguments" withoutSafe
              >>= (@?= ([], [], [], []))
        , testCase "hidden metavariables are reported alongside goals" $ do
            response <-
              withFixtureSession "test/fixtures/Normalization.agda" $ \path ->
                load Request {requestPath = path, requestArguments = []}
            (goals, hiddenMetavariables, warnings, nonFatalErrors) <-
              expectLoaded "load" response
            map goalShape goals @?= [GoalOfType "Twice 2"]
            map hiddenMetavariableShape hiddenMetavariables @?= [GoalOfType "Twice 2"]
            map (isJust . hiddenMetavariableSpan) hiddenMetavariables @?= [True]
            warnings @?= []
            nonFatalErrors @?= []
        , -- Test a claim in `extractResponseOk`
          testCase "AsIs normalization is weaker than Simplified" $
            assertBool
              "AsIs should be less than or equal to Simplified"
              (AsIs <= Simplified)
        ]
    ]

brokenHoleNatural :: Text
brokenHoleNatural =
  Text.unlines
    [ "module HoleNatural where"
    , ""
    , "open import Data.Nat"
    , ""
    , "foo : ℕ → ℕ"
    , "foo n = ℕ"
    ]
