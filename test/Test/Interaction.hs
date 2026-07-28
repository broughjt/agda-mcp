{-# LANGUAGE OverloadedStrings #-}

module Test.Interaction (tests) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Interaction.Response (InteractionOutputCallback, Response_boot (..))
import Agda.TypeChecking.Monad (initEnv, runTCM, setInteractionOutputCallback)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (runStateT)
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time (addUTCTime)
import System.Directory (getModificationTime, setModificationTime)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (
  assertBool,
  assertEqual,
  assertFailure,
  testCase,
  (@?=),
 )

import AgdaMCP.Interaction (InteractionM, newInteractionState)
import AgdaMCP.Interaction.Context (context)
import AgdaMCP.Interaction.Context qualified as Context
import AgdaMCP.Interaction.Goal (goal)
import AgdaMCP.Interaction.Goal qualified as Goal
import AgdaMCP.Interaction.Internal (InteractionState (..))
import AgdaMCP.Interaction.Intro (intro)
import AgdaMCP.Interaction.Intro qualified as Intro
import AgdaMCP.Interaction.Load (load)
import AgdaMCP.Interaction.Load qualified as Load
import AgdaMCP.Interaction.Model (
  ContextEntry (..),
  Error (..),
  GiveAction (..),
  Goal (..),
  GoalReport (..),
  GoalShape (..),
  HiddenMetavariable (..),
  NonFatalError (..),
  Warning (..),
 )
import AgdaMCP.Interaction.Refine (refine)
import AgdaMCP.Interaction.Refine qualified as Refine
import Test.Harness (
  currentFile,
  expectLoadError,
  expectLoadOk,
  spanCoordinates,
  spanText,
  withFixtureDirectory,
  withFixtureSession,
  withStagedFiles,
 )

tests :: TestTree
tests = testGroup "interaction" [loadTests, scenarioTests]

loadTests :: TestTree
loadTests =
  testGroup
    "Load"
    [ testCase "load a file with single hole of type ℕ" $ do
        response <-
          withFixtureSession "test/fixtures/HoleNatural.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        case response of
          Load.ResponseOk goals hiddenMetavariables warnings nonFatalErrors -> do
            map (\g -> (goalId g, goalShape g)) goals @?= [(0, GoalOfType "ℕ")]
            hiddenMetavariables @?= []
            warnings @?= []
            nonFatalErrors @?= []
          other ->
            assertFailure $ "expected ResponseOk, got " <> show other
    , testCase "load a file with three holes" $ do
        response <-
          withFixtureSession "test/fixtures/GroupProperties.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        case response of
          Load.ResponseOk goals hiddenMetavariables warnings nonFatalErrors -> do
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
            response <-
              load Load.Request {Load.requestPath = path, Load.requestArguments = []}
            void $ liftIO $ expectLoadOk "load" response
            (,) path <$> currentFile
        recorded @?= Just path
    , testCase "a failed load clears the recorded current file" $ do
        (afterSuccess, afterFailure) <-
          withFixtureSession "test/fixtures/HoleNatural.agda" $ \path -> do
            let request = Load.Request {Load.requestPath = path, Load.requestArguments = []}
            void $ load request >>= liftIO . expectLoadOk "initial load"
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
              let request = Load.Request {Load.requestPath = path, Load.requestArguments = []}
              original <- liftIO $ Text.IO.readFile path
              before <- load request
              void $ liftIO $ expectLoadOk "initial load" before
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
              load Load.Request {Load.requestPath = path, Load.requestArguments = ["--safe"]}
            withoutSafe <-
              load Load.Request {Load.requestPath = path, Load.requestArguments = []}
            pure (withSafe, withoutSafe)
        -- `--safe` rejects the postulate as a non-fatal error
        (goals, hiddenMetavariables, warnings, nonFatalErrors) <-
          expectLoadOk "load with --safe" withSafe
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
        expectLoadOk "load without arguments" withoutSafe
          >>= (@?= ([], [], [], []))
    , testCase "hidden metavariables are reported alongside goals" $ do
        response <-
          withFixtureSession "test/fixtures/Normalization.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        (goals, hiddenMetavariables, warnings, nonFatalErrors) <-
          expectLoadOk "load" response
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
            (,) path
              <$> load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        (goals, hiddenMetavariables, warnings, nonFatalErrors) <-
          expectLoadOk "load" response
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
              <$> load Load.Request {Load.requestPath = path, Load.requestArguments = []}
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
            (,) path
              <$> load Load.Request {Load.requestPath = path, Load.requestArguments = []}
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
              let request = Load.Request {Load.requestPath = path, Load.requestArguments = []}
              response1 <- load request
              maybeCurrentFile <- currentFile
              liftIO stopModifying
              response2 <- load request
              pure (response1, maybeCurrentFile, response2)
        response1 @?= Load.ResponseStale
        maybeCurrentFile @?= Nothing
        void $ expectLoadOk "load after the file settles" response2
    , -- Agda checks a `where` module before the clause body containing it,
      -- so the hole on line 9 is created first and gets the lower id.
      -- `extractResponseOk` sorts by position, so the ids come back
      -- descending here.
      testCase "goals are ordered by position, not by interaction id" $ do
        response <-
          withFixtureSession "test/fixtures/GoalOrder.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        (goals, _, _, _) <- expectLoadOk "load" response
        map (spanCoordinates . goalSpan) goals
          @?= [((6, 11), (6, 15)), ((9, 11), (9, 15))]
        map goalId goals @?= [1, 0]
    , -- `extractVisibleMetavariable` renders with `prettyATop` rather than
      -- the JSON frontend's `prettyTCM`, which would parenthesize a
      -- function type sitting in an argument position.
      testCase "goal type rendering" $ do
        response <-
          withFixtureSession "test/fixtures/Parenthesization.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        (goals, _, _, _) <- expectLoadOk "load" response
        map goalShape goals @?= [GoalOfType "ℕ → ℕ"]
    , testCase "a sort-shaped hidden metavariable is reported" $ do
        response <-
          withFixtureSession "test/fixtures/SortMetavariable.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        (goals, hiddenMetavariables, _, _) <- expectLoadOk "load" response
        map goalShape goals @?= [GoalOfType "_0"]
        map hiddenMetavariableName hiddenMetavariables @?= ["_0"]
        map hiddenMetavariableShape hiddenMetavariables @?= [GoalSort]
    , testCase "a warning from an imported module keeps its own path" $ do
        (directory, response) <-
          withFixtureDirectory "test/fixtures/imported-warning" $ \directory ->
            (,) directory
              <$> load
                Load.Request
                  { Load.requestPath = directory </> "Importer.agda"
                  , Load.requestArguments = []
                  }
        (goals, _, warnings, nonFatalErrors) <- expectLoadOk "load" response
        goals @?= []
        nonFatalErrors @?= []
        map warningLocation warnings
          @?= [Just (directory </> "Warned.agda", ((8, 1), (8, 12)))]
    , testCase "importing a module with open holes fails" $ do
        (directory, response) <-
          withFixtureDirectory "test/fixtures/open-holes" $ \directory ->
            (,) directory
              <$> load
                Load.Request
                  { Load.requestPath = directory </> "HoleImporter.agda"
                  , Load.requestArguments = []
                  }
        e <- expectLoadError "load" response
        assertBool
          ("expected open interaction points, got " <> Text.unpack (errorMessage e))
          ("open interaction points" `Text.isInfixOf` errorMessage e)
        fmap (fmap spanCoordinates) (errorPathSpan e)
          @?= Just (directory </> "HoleImporter.agda", ((3, 1), (3, 32)))
    , testCase "loading a missing file reports an error" $ do
        response <-
          withFixtureSession "test/fixtures/HoleNatural.agda" $ \path ->
            load
              Load.Request
                { Load.requestPath = takeDirectory path </> "Missing.agda"
                , Load.requestArguments = []
                }
        e <- expectLoadError "load" response
        assertBool
          ("expected a read failure, got " <> Text.unpack (errorMessage e))
          ("Cannot read file" `Text.isInfixOf` errorMessage e)
    ]

scenarioTests :: TestTree
scenarioTests =
  testGroup
    "Scenarios"
    [ -- Builds `+-assoc` the way a caller would. Inspect the goals, read a
      -- context, close the base case with `intro`, refine the inductive
      -- step, then fill what refine leaves remaining. Nothing is written to
      -- disk, so this is entirely session state.
      testCase "build a proof of addition associativity" $
        withFixtureSession "test/fixtures/ProofScenario.agda" $ \path -> do
          let request = Load.Request {Load.requestPath = path, Load.requestArguments = []}
              step label expected actual = liftIO $ assertEqual label expected actual

          goals <-
            load request >>= liftIO . fmap goalsOf . expectLoadOk "load"
          step
            "the two clauses each leave a hole"
            [ GoalOfType "zero + n + p ≡ zero + (n + p)"
            , GoalOfType "suc m + n + p ≡ suc m + (n + p)"
            ]
            (map goalShape goals)

          stepContext <-
            context
              Context.Request
                { Context.requestNormalization = AsIs
                , Context.requestGoalId = 1
                }
          step
            "the inductive step binds the three arguments"
            (Right (["m", "n", "p"], ["ℕ", "ℕ", "ℕ"]))
            ( fmap
                (\entries -> (map contextEntryOriginalName entries, map contextEntryType entries))
                stepContext
            )

          baseReport <-
            goal
              Goal.Request
                { Goal.requestNormalization = AsIs
                , Goal.requestGoalId = 0
                }
          step
            "the base case sees only the arguments its pattern binds"
            (Right (GoalOfType "zero + n + p ≡ zero + (n + p)", ["n", "p"]))
            ( fmap
                (\r -> (goalReportShape r, map contextEntryOriginalName (goalReportContext r)))
                baseReport
            )

          -- The identity type has one constructor, so `intro` is
          -- unambiguous here.
          introduced <-
            intro
              Intro.Request
                { Intro.requestPatternLambda = False
                , Intro.requestGoalId = 0
                }
          step "intro closes the base case" (Right (GiveComputed "refl")) introduced

          refined <-
            refine
              Refine.Request
                { Refine.requestGoalId = 1
                , Refine.requestExpression = "cong suc"
                }
          step
            "refine leaves a hole for the recursive call"
            (Right (GiveComputed "cong suc ?"))
            refined

          -- Refine's new hole exists in the session immediately, without
          -- writing the splice to disk and reloading.
          recursiveCall <-
            goal
              Goal.Request
                { Goal.requestNormalization = AsIs
                , Goal.requestGoalId = 2
                }
          step
            "the hole refine introduced is the recursive call"
            (Right (GoalOfType "m + n + p ≡ m + (n + p)"))
            (fmap goalReportShape recursiveCall)

          -- The file on disk never changed, so a reload throws all of that
          -- away and returns the original two goals.
          reloaded <-
            load request >>= liftIO . fmap goalsOf . expectLoadOk "reload"
          step
            "reloading discards the session's progress"
            (map goalShape goals)
            (map goalShape reloaded)
    ]

goalsOf :: ([Goal], [HiddenMetavariable], [Warning], [NonFatalError]) -> [Goal]
goalsOf (goals, _, _, _) = goals

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
-- again after, treating a difference as "the file changed under us". The second
-- argument disarms file modification so that a later load in the same session
-- succeeds.
withStaleFixtureSession ::
  FilePath -> (FilePath -> IO () -> InteractionM a) -> IO a
withStaleFixtureSession source k =
  withStagedFiles [source] $ \directory options -> do
    let staged = directory </> takeFileName source
    armed <- newIORef True
    state <- newInteractionState options >>= touchWhileChecking armed staged
    fst <$> runStateT (k staged (writeIORef armed False)) state

-- Agda emits `Resp_RunningInfo` from `chaseMsg` while type checking, between
-- the two modification time samples.
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
