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

import System.FilePath (takeDirectory, (</>))

import AgdaMCP.Interaction.Load (Request (..), Response (..), load)
import AgdaMCP.Interaction.Model (
  Error (..),
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  NonFatalError (..),
  Warning (..),
 )
import Data.Functor (void)
import Test.Harness (
  currentFile,
  expectLoadError,
  expectLoaded,
  spanCoordinates,
  spanText,
  withFixtureSession,
  withStaleFixtureSession,
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
        , testCase "warnings are reported in file order with their locations" $ do
            (path, response) <-
              withFixtureSession "test/fixtures/Warnings.agda" $ \path ->
                (,) path <$> load Request {requestPath = path, requestArguments = []}
            (goals, hiddenMetavariables, warnings, nonFatalErrors) <-
              expectLoaded "load" response
            goals @?= []
            hiddenMetavariables @?= []
            nonFatalErrors @?= []
            map warningLocation warnings
              @?= [ Just (path, ((8, 1), (8, 12)))
                  , Just (path, ((13, 1), (13, 13)))
                  ]
            map (withoutPath path . firstLine . warningMessage) warnings
              @?= [ "FIXTURE:8.1-12: warning: -W[no]UnreachableClauses"
                  , "FIXTURE:13.1-13: warning: -W[no]UnreachableClauses"
                  ]
        , testCase "a type error reports its message and span" $ do
            (path, source, response) <-
              withFixtureSession "test/fixtures/TypeError.agda" $ \path -> do
                source <- liftIO $ Text.IO.readFile path
                (,,) path source
                  <$> load Request {requestPath = path, requestArguments = []}
            e <- expectLoadError "load" response
            assertBool
              ("expected an UnequalTerms error, got " <> Text.unpack (errorMessage e))
              ("UnequalTerms" `Text.isInfixOf` errorMessage e)
            case errorPathSpan e of
              Just (errorPath, s) -> do
                errorPath @?= path
                spanCoordinates s @?= ((6, 9), (6, 10))
                spanText source s @?= "ℕ"
              Nothing ->
                assertFailure "expected the error to carry a span"
        , testCase "a failed load carries the warnings raised before the error" $ do
            (path, response) <-
              withFixtureSession "test/fixtures/WarningThenError.agda" $ \path ->
                (,) path <$> load Request {requestPath = path, requestArguments = []}
            e <- expectLoadError "load" response
            assertBool
              ("expected an UnequalTerms error, got " <> Text.unpack (errorMessage e))
              ("UnequalTerms" `Text.isInfixOf` errorMessage e)
            assertBool
              "the error message should not also contain the warning"
              (not $ "UnreachableClauses" `Text.isInfixOf` errorMessage e)
            map warningLocation (errorWarnings e)
              @?= [Just (path, ((8, 1), (8, 12)))]
            map (withoutPath path . firstLine . warningMessage) (errorWarnings e)
              @?= ["FIXTURE:8.1-12: warning: -W[no]UnreachableClauses"]
        , testCase "a file changed while it is checked loads as stale" $ do
            (response1, maybeCurrentFile, response2) <-
              withStaleFixtureSession "test/fixtures/HoleNatural.agda" $
                \path stopModifying -> do
                  let request = Request {requestPath = path, requestArguments = []}
                  response1 <- load request
                  maybeCurrentFile <- currentFile
                  liftIO stopModifying
                  response2 <- load request
                  pure (response1, maybeCurrentFile, response2)
            response1 @?= ResponseStale
            maybeCurrentFile @?= Nothing
            void $ expectLoaded "load after the file settles" response2
        , testCase "loading a missing file reports an error" $ do
            response <-
              withFixtureSession "test/fixtures/HoleNatural.agda" $ \path ->
                load
                  Request
                    { requestPath = takeDirectory path </> "Missing.agda"
                    , requestArguments = []
                    }
            e <- expectLoadError "load" response
            assertBool
              ("expected a read failure, got " <> Text.unpack (errorMessage e))
              ("Cannot read file" `Text.isInfixOf` errorMessage e)
        ]
    ]

warningMessage :: Warning -> Text
warningMessage (Warning (_, message)) = message

warningLocation :: Warning -> Maybe (FilePath, ((Int, Int), (Int, Int)))
warningLocation (Warning (pathSpan, _)) =
  (\(path, s) -> (path, spanCoordinates s)) <$> pathSpan

firstLine :: Text -> Text
firstLine = Text.takeWhile (/= '\n')

-- Fixtures are staged in a fresh temporary directory, so the path Agda embeds
-- in rendered messages differs on every run.
withoutPath :: FilePath -> Text -> Text
withoutPath path = Text.replace (Text.pack path) "FIXTURE"

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

-- `cmd_load'` samples the fixture's modification time before type checking and
-- again after, and treats a difference as "the file changed under us". The
-- second argument to the continuation is an action to disarm file modification
-- so that later loads in the same session can succeed.
withStaleFixtureSession ::
  FilePath -> (FilePath -> IO () -> InteractionM a) -> IO a
withStaleFixtureSession source k =
  withStagedFixture source $ \staged options -> do
    armed <- newIORef True
    state <- newInteractionState options >>= touchWhileChecking armed staged
    fst <$> runStateT (k staged (writeIORef armed False)) state

-- Agda emits `Resp_RunningInfo` from `chaseMsg` while type checking, which is
-- between the two modification time samples.
touchWhileChecking ::
  IORef Bool -> FilePath -> InteractionState -> IO InteractionState
touchWhileChecking armed path (InteractionState tcState commandState slot) = do
  ((), tcState') <-
    runTCM initEnv tcState $ setInteractionOutputCallback callback
  pure $ InteractionState tcState' commandState slot
 where
  callback :: InteractionOutputCallback
  callback (Resp_RunningInfo _ _) = liftIO $ do
    shouldTouch <- readIORef armed
    when shouldTouch $
      getModificationTime path >>= setModificationTime path . addUTCTime 1
  callback _ = pure ()
