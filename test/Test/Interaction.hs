{-# LANGUAGE OverloadedStrings #-}

module Test.Interaction (tests) where

import Agda.Interaction.Base (
  CurrentFile (..),
  Rewrite (..),
  UseForce (..),
  theCurrentFile,
 )
import Agda.Interaction.Response (InteractionOutputCallback, Response_boot (..))
import Agda.Syntax.Common (InteractionId)
import Agda.TypeChecking.Monad (
  TCState,
  initEnv,
  runTCM,
  setInteractionOutputCallback,
 )
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, runStateT)
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time (addUTCTime)
import System.Directory (getModificationTime, setModificationTime)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (
  assertBool,
  assertEqual,
  assertFailure,
  testCase,
  (@?=),
 )

import Agda.Utils.FileName (filePath)
import AgdaMCP.Interaction (InteractionM)
import AgdaMCP.Interaction.Context (context)
import AgdaMCP.Interaction.Context qualified as Context
import AgdaMCP.Interaction.ElaborateGive (elaborateGive)
import AgdaMCP.Interaction.ElaborateGive qualified as ElaborateGive
import AgdaMCP.Interaction.Give (give)
import AgdaMCP.Interaction.Give qualified as Give
import AgdaMCP.Interaction.Goal (goal)
import AgdaMCP.Interaction.Goal qualified as Goal
import AgdaMCP.Interaction.Internal (InteractionState (..))
import AgdaMCP.Interaction.Intro (intro)
import AgdaMCP.Interaction.Intro qualified as Intro
import AgdaMCP.Interaction.Load (load)
import AgdaMCP.Interaction.Load qualified as Load
import AgdaMCP.Interaction.Metas (metas)
import AgdaMCP.Interaction.Metas qualified as Metas
import AgdaMCP.Interaction.Model (
  ContextEntry (..),
  Error (..),
  GiveAction (..),
  GiveError (..),
  Goal (..),
  GoalError (..),
  GoalReport (..),
  GoalShape (..),
  HiddenMetavariable (..),
  IntroError (..),
  MetasReport (..),
  NonFatalError (..),
  Position (..),
  Span (..),
  Warning (..),
 )
import AgdaMCP.Interaction.Refine (refine)
import AgdaMCP.Interaction.Refine qualified as Refine
import Test.Harness (
  warmInteractionState,
  warmedSession,
  withFixtureDirectory,
  withFixtureSession,
  withStagedFiles,
 )

tests :: TestTree
tests =
  withResource warmInteractionState (const $ pure ()) $ \warm ->
    testGroup
      "interaction"
      ( map
          ($ warm)
          [ loadTests
          , metasTests
          , contextTests
          , giveFamilyTests
          , giveTests
          , refineTests
          , introTests
          , elaborateGiveTests
          , scenarioTests
          ]
      )

loadTests :: IO TCState -> TestTree
loadTests warm =
  testGroup
    "Load"
    [ testCase "load a file with single hole of type ℕ" $ do
        response <-
          withFixtureSession warm "test/fixtures/HoleNatural.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        report <- expectLoadOk "load" response
        map (\g -> (goalId g, goalShape g)) (metasReportGoals report)
          @?= [(0, GoalOfType "ℕ")]
        metasReportHiddenMetavariables report @?= []
        metasReportWarnings report @?= []
        metasReportNonFatalErrors report @?= []
    , testCase "load a file with three holes" $ do
        response <-
          withFixtureSession warm "test/fixtures/GroupProperties.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        report <- expectLoadOk "load" response
        map goalId (metasReportGoals report) @?= [0, 1, 2]
        map goalShape (metasReportGoals report)
          @?= [ GoalOfType "x ≈ y → u ≈ v → x // u ≈ y // v"
              , GoalOfType "(setoid Relation.Binary.Bundles.Setoid.≈ (y ∙ ε)) y"
              , GoalOfType "ε ⁻¹ ≈ ε"
              ]
        metasReportHiddenMetavariables report @?= []
        metasReportWarnings report @?= []
        metasReportNonFatalErrors report @?= []
    , testCase "a successful load records the current file" $ do
        (path, recorded) <-
          withFixtureSession warm "test/fixtures/HoleNatural.agda" $ \path -> do
            response <-
              load Load.Request {Load.requestPath = path, Load.requestArguments = []}
            void $ liftIO $ expectLoadOk "load" response
            (,) path <$> currentFile
        recorded @?= Just path
    , testCase "a failed load clears the recorded current file" $ do
        (afterSuccess, afterFailure) <-
          withFixtureSession warm "test/fixtures/HoleNatural.agda" $ \path -> do
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
            withFixtureSession warm "test/fixtures/HoleNatural.agda" $ \path -> do
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
          withFixtureSession warm "test/fixtures/SafePostulate.agda" $ \path -> do
            withSafe <-
              load Load.Request {Load.requestPath = path, Load.requestArguments = ["--safe"]}
            withoutSafe <-
              load Load.Request {Load.requestPath = path, Load.requestArguments = []}
            pure (withSafe, withoutSafe)
        -- `--safe` rejects the postulate as a non-fatal error
        report <- expectLoadOk "load with --safe" withSafe
        metasReportGoals report @?= []
        metasReportHiddenMetavariables report @?= []
        metasReportWarnings report @?= []
        case metasReportNonFatalErrors report of
          [NonFatalError (_, message)] ->
            assertBool
              ("expected a SafeFlagPostulate error, got " <> Text.unpack message)
              ("SafeFlagPostulate" `Text.isInfixOf` message)
          other ->
            assertFailure $ "expected one non-fatal error, got " <> show other
        expectLoadOk "load without arguments" withoutSafe
          >>= (@?= MetasReport [] [] [] [])
    , testCase "hidden metavariables are reported alongside goals" $ do
        response <-
          withFixtureSession warm "test/fixtures/Normalization.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        report <- expectLoadOk "load" response
        map goalShape (metasReportGoals report) @?= [GoalOfType "Twice 2"]
        map hiddenMetavariableShape (metasReportHiddenMetavariables report)
          @?= [GoalOfType "Twice 2"]
        map (isJust . hiddenMetavariableSpan) (metasReportHiddenMetavariables report)
          @?= [True]
        metasReportWarnings report @?= []
        metasReportNonFatalErrors report @?= []
    , testCase "warnings are reported in file order with their locations" $ do
        (path, response) <-
          withFixtureSession warm "test/fixtures/Warnings.agda" $ \path ->
            (,) path
              <$> load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        report <- expectLoadOk "load" response
        metasReportGoals report @?= []
        metasReportHiddenMetavariables report @?= []
        metasReportNonFatalErrors report @?= []
        map warningLocation (metasReportWarnings report)
          @?= [ Just (path, ((8, 1), (8, 12)))
              , Just (path, ((13, 1), (13, 13)))
              ]
        map
          (withoutPath path . firstLine . warningMessage)
          (metasReportWarnings report)
          @?= [ "FIXTURE:8.1-12: warning: -W[no]UnreachableClauses"
              , "FIXTURE:13.1-13: warning: -W[no]UnreachableClauses"
              ]
    , testCase "a type error reports its message and span" $ do
        (path, source, response) <-
          withFixtureSession warm "test/fixtures/TypeError.agda" $ \path -> do
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
          withFixtureSession warm "test/fixtures/WarningThenError.agda" $ \path ->
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
          withStaleFixtureSession warm "test/fixtures/HoleNatural.agda" $
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
      -- `extractMetas` sorts by position, so the ids come back
      -- descending here.
      testCase "goals are ordered by position, not by interaction id" $ do
        response <-
          withFixtureSession warm "test/fixtures/GoalOrder.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        goals <- metasReportGoals <$> expectLoadOk "load" response
        map (spanCoordinates . goalSpan) goals
          @?= [((6, 11), (6, 15)), ((9, 11), (9, 15))]
        map goalId goals @?= [1, 0]
    , -- `extractVisibleMetavariable` renders with `prettyATop` rather than
      -- the JSON frontend's `prettyTCM`, which would parenthesize a
      -- function type sitting in an argument position.
      testCase "goal type rendering" $ do
        response <-
          withFixtureSession warm "test/fixtures/Parenthesization.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        goals <- metasReportGoals <$> expectLoadOk "load" response
        map goalShape goals @?= [GoalOfType "ℕ → ℕ"]
    , testCase "a sort-shaped hidden metavariable is reported" $ do
        response <-
          withFixtureSession warm "test/fixtures/SortMetavariable.agda" $ \path ->
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
        report <- expectLoadOk "load" response
        let goals = metasReportGoals report
            hiddenMetavariables = metasReportHiddenMetavariables report
        map goalShape goals @?= [GoalOfType "_0"]
        map hiddenMetavariableName hiddenMetavariables @?= ["_0"]
        map hiddenMetavariableShape hiddenMetavariables @?= [GoalSort]
    , testCase "a warning from an imported module keeps its own path" $ do
        (directory, response) <-
          withFixtureDirectory warm "test/fixtures/imported-warning" $ \directory ->
            (,) directory
              <$> load
                Load.Request
                  { Load.requestPath = directory </> "Importer.agda"
                  , Load.requestArguments = []
                  }
        report <- expectLoadOk "load" response
        metasReportGoals report @?= []
        metasReportNonFatalErrors report @?= []
        map warningLocation (metasReportWarnings report)
          @?= [Just (directory </> "Warned.agda", ((8, 1), (8, 12)))]
    , testCase "importing a module with open holes fails" $ do
        (directory, response) <-
          withFixtureDirectory warm "test/fixtures/open-holes" $ \directory ->
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
          withFixtureSession warm "test/fixtures/HoleNatural.agda" $ \path ->
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

metasTests :: IO TCState -> TestTree
metasTests warm =
  testGroup
    "Metas"
    [ testCase "hidden metavariables normalize at least as far as Simplified" $
        withFixtureSession warm "test/fixtures/Normalization.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"
          shapes <-
            traverse
              ( \normalization -> do
                  report <-
                    metas Metas.Request {Metas.requestNormalization = normalization}
                      >>= liftIO . expectMetasOk (show normalization)
                  pure
                    ( map goalShape (metasReportGoals report)
                    , map
                        hiddenMetavariableShape
                        (metasReportHiddenMetavariables report)
                    )
              )
              [AsIs, Instantiated, HeadNormal, Simplified, Normalised]
          liftIO $
            shapes
              @?= [ ([GoalOfType "Twice 2"], [GoalOfType "Twice 2"])
                  , ([GoalOfType "Twice 2"], [GoalOfType "Twice 2"])
                  , ([GoalOfType "P (2 + 2)"], [GoalOfType "Twice 2"])
                  , ([GoalOfType "Twice 2"], [GoalOfType "Twice 2"])
                  , ([GoalOfType "P 4"], [GoalOfType "P 4"])
                  ]
    , testCase "a metavariable stranded by a give is invisible to a reload" $
        withFixtureSession warm "test/fixtures/GiveExpressions.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"
          void $
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 6
                , Give.requestExpression = "_"
                }
              >>= liftIO . expectGiveOk "an underscore"

          report <-
            metas Metas.Request {Metas.requestNormalization = AsIs}
              >>= liftIO . expectMetasOk "after the give"
          liftIO $ do
            map goalId (metasReportGoals report) @?= [0, 1, 2, 3, 4, 5]
            map hiddenMetavariableShape (metasReportHiddenMetavariables report)
              @?= [GoalOfType "ℕ"]

          reloadedReport <-
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "reload"
          liftIO $ metasReportHiddenMetavariables reloadedReport @?= []
    , testCase "metas reports a non-fatal error raised by a give" $ do
        let attempt force =
              withFixtureSession warm "test/fixtures/Termination.agda" $ \path -> do
                void $
                  load Load.Request {Load.requestPath = path, Load.requestArguments = []}
                    >>= liftIO . expectLoadOk "load"
                void $
                  give
                    Give.Request
                      { Give.requestForce = force
                      , Give.requestGoalId = 0
                      , Give.requestExpression = "loop n"
                      }
                    >>= liftIO . expectGiveOk "a non-terminating expression"
                report <-
                  metas Metas.Request {Metas.requestNormalization = AsIs}
                    >>= liftIO . expectMetasOk "after the give"
                pure $
                  map
                    (firstLine . nonFatalErrorMessage)
                    (metasReportNonFatalErrors report)
        withoutForce <- attempt WithoutForce
        withForce <- attempt WithForce
        assertBool
          ("expected a termination issue, got " <> show withoutForce)
          (any ("TerminationIssue" `Text.isInfixOf`) withoutForce)
        withForce @?= []
    ]

contextTests :: IO TCState -> TestTree
contextTests warm =
  testGroup
    "Context"
    [ testCase "the context lists locals outermost first, then let bindings" $
        withFixtureSession warm "test/fixtures/ContextBindings.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          -- `m` and `n` are locals with the let binding between them in the
          -- telescope, so this distinguishes telescope order from the order
          -- `contextOfMeta` actually reports.
          entries <-
            context
              Context.Request
                { Context.requestNormalization = AsIs
                , Context.requestGoalId = 0
                }
              >>= liftIO . expectContextOk "a clause mixing binders and a let"
          liftIO $
            map
              ( \entry ->
                  ( contextEntryOriginalName entry
                  , contextEntryType entry
                  , contextEntryLetValue entry
                  )
              )
              entries
              @?= [ ("m", "ℕ", Nothing)
                  , ("n", "ℕ", Nothing)
                  , ("doubled", "ℕ", Just "m + m")
                  ]
    , testCase "a shadowed binding keeps its original name beside the reified one" $
        withFixtureSession warm "test/fixtures/ContextBindings.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          -- Agda renders these two as `x = x₁ : ℕ   (not in scope)` and
          -- `x : ℕ`. The outer binder took the second display form, so its
          -- original name is in scope while the reified name it was given is
          -- not.
          entries <-
            context
              Context.Request
                { Context.requestNormalization = AsIs
                , Context.requestGoalId = 1
                }
              >>= liftIO . expectContextOk "a clause whose binder is shadowed"
          liftIO $
            map
              ( \entry ->
                  ( contextEntryOriginalName entry
                  , contextEntryReifiedName entry
                  , contextEntryOriginalInScope entry
                  , contextEntryReifiedInScope entry
                  )
              )
              entries
              @?= [ ("x", "x₁", True, False)
                  , ("x", "x", True, True)
                  ]
    , testCase "an anonymous binding gets a name that is not in scope" $
        withFixtureSession warm "test/fixtures/ContextBindings.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          entries <-
            context
              Context.Request
                { Context.requestNormalization = AsIs
                , Context.requestGoalId = 3
                }
              >>= liftIO . expectContextOk "a clause with an anonymous binder"
          liftIO $
            map
              ( \entry ->
                  ( contextEntryOriginalName entry
                  , contextEntryReifiedName entry
                  , contextEntryOriginalInScope entry
                  , contextEntryReifiedInScope entry
                  )
              )
              entries
              @?= [ ("x", "x", False, False)
                  , ("n", "n", True, True)
                  ]
    , testCase "entry types are rendered at the requested normalization" $
        withFixtureSession warm "test/fixtures/ContextBindings.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          types <-
            traverse
              ( \normalization ->
                  context
                    Context.Request
                      { Context.requestNormalization = normalization
                      , Context.requestGoalId = 2
                      }
                    >>= liftIO
                      . fmap (map contextEntryType)
                      . expectContextOk (show normalization)
              )
              [AsIs, Instantiated, HeadNormal, Simplified, Normalised]
          -- The same table the goal types produce: `simplify` collapses onto
          -- `AsIs` even where `HeadNormal` reduces.
          liftIO $
            types
              @?= [ ["Twice 2"]
                  , ["Twice 2"]
                  , ["P (2 + 2)"]
                  , ["Twice 2"]
                  , ["P 4"]
                  ]
    , testCase "binder attributes are reported per entry" $
        withFixtureSession warm "test/fixtures/ContextAttributes.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          let attributes entry =
                ( contextEntryOriginalName entry
                ,
                  ( contextEntryIsInstance entry
                  , contextEntryErased entry
                  , contextEntryRelevance entry
                  , -- Nothing in this fixture is annotated with either, and
                    -- exercising them would mean turning on further language
                    -- features, so they stay absent here.
                    contextEntryCohesion entry
                  , contextEntryPolarity entry
                  )
                )
              entriesFor label goalId =
                context
                  Context.Request
                    { Context.requestNormalization = AsIs
                    , Context.requestGoalId = goalId
                    }
                  >>= liftIO . fmap (map attributes) . expectContextOk label

          instanceArgument <- entriesFor "an instance argument" 0
          liftIO $
            instanceArgument
              @?= [ ("eq", (True, False, Nothing, Nothing, Nothing))
                  , ("n", (False, False, Nothing, Nothing, Nothing))
                  ]

          erasedArgument <- entriesFor "an erased argument" 1
          liftIO $
            erasedArgument @?= [("n", (False, True, Nothing, Nothing, Nothing))]

          irrelevantArgument <- entriesFor "an irrelevant argument" 2
          liftIO $
            irrelevantArgument
              @?= [("n", (False, False, Just "irrelevant", Nothing, Nothing))]
    , testCase "a bogus goal id is rejected" $
        withFixtureSession warm "test/fixtures/ContextBindings.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          response <-
            context
              Context.Request
                { Context.requestNormalization = AsIs
                , Context.requestGoalId = bogusGoalId
                }
          liftIO $ response @?= Left (GoalUnknownId bogusGoalId)
    ]

giveFamilyTests :: IO TCState -> TestTree
giveFamilyTests warm =
  testGroup
    "Give family"
    [ testCase "bogus goal id case for give family" $
        withFixtureSession warm "test/fixtures/GiveFamily.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          given <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = bogusGoalId
                , Give.requestExpression = "zero"
                }
              >>= liftIO . expectGiveError "give"
          liftIO $ given @?= GiveUnknownId bogusGoalId

          refined <-
            refine
              Refine.Request
                { Refine.requestGoalId = bogusGoalId
                , Refine.requestExpression = "suc"
                }
              >>= liftIO . expectGiveError "refine"
          liftIO $ refined @?= GiveUnknownId bogusGoalId

          introduced <-
            intro
              Intro.Request
                { Intro.requestPatternLambda = False
                , Intro.requestGoalId = bogusGoalId
                }
              >>= liftIO . expectIntroError "intro"
          liftIO $ introduced @?= IntroUnknownId bogusGoalId

          elaborated <-
            elaborateGive
              ElaborateGive.Request
                { ElaborateGive.requestNormalization = Normalised
                , ElaborateGive.requestGoalId = bogusGoalId
                , ElaborateGive.requestExpression = "zero"
                }
              >>= liftIO . expectGiveError "elaborateGive"
          liftIO $ elaborated @?= GiveUnknownId bogusGoalId
    , testCase "success case for give family" $
        withFixtureSession warm "test/fixtures/GiveFamily.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          given <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 0
                , Give.requestExpression = "zero"
                }
              >>= liftIO . expectGiveOk "give keeps the user's own text"
          liftIO $ given @?= GiveVerbatim False

          refined <-
            refine
              Refine.Request
                { Refine.requestGoalId = 1
                , Refine.requestExpression = "suc"
                }
              >>= liftIO . expectGiveOk "refine leaves a hole"
          liftIO $ refined @?= GiveComputed "suc ?"

          introduced <-
            intro
              Intro.Request
                { Intro.requestPatternLambda = False
                , Intro.requestGoalId = 2
                }
              >>= liftIO . expectIntroOk "intro picks the sole constructor"
          liftIO $ introduced @?= "tt"

          elaborated <-
            elaborateGive
              ElaborateGive.Request
                { ElaborateGive.requestNormalization = Normalised
                , ElaborateGive.requestGoalId = 3
                , ElaborateGive.requestExpression = "1 + 1"
                }
              >>= liftIO . expectElaborated "elaborateGive normalizes"
          liftIO $ elaborated @?= "2"
    , testCase "a failed give family command leaves its goal usable" $
        withFixtureSession warm "test/fixtures/GiveFamily.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          e <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 0
                , Give.requestExpression = "suc suc"
                }
              >>= liftIO . expectGiveFailure "ill-typed give"
          liftIO $
            assertBool
              ( "ill-typed give: expected \"UnequalTerms\" within "
                  <> show (errorMessage e)
              )
              ("UnequalTerms" `Text.isInfixOf` errorMessage e)

          retried <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 0
                , Give.requestExpression = "zero"
                }
              >>= liftIO . expectGiveOk "the same goal accepts a later give"
          liftIO $ retried @?= GiveVerbatim False
    ]

giveTests :: IO TCState -> TestTree
giveTests warm =
  testGroup
    "Give"
    [ testCase "give classifies the ways an expression can be rejected" $
        withFixtureSession warm "test/fixtures/GiveExpressions.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          illTyped <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 0
                , Give.requestExpression = "suc suc"
                }
              >>= liftIO . expectGiveFailure "ill-typed"
          liftIO $
            assertBool
              ( "ill-typed: expected \"UnequalTerms\" within "
                  <> show (errorMessage illTyped)
              )
              ("UnequalTerms" `Text.isInfixOf` errorMessage illTyped)

          outOfScope <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 1
                , Give.requestExpression = "nope"
                }
              >>= liftIO . expectGiveFailure "out of scope"
          liftIO $
            assertBool
              ( "out of scope: expected \"NotInScope\" within "
                  <> show (errorMessage outOfScope)
              )
              ("NotInScope" `Text.isInfixOf` errorMessage outOfScope)

          unparseable <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 2
                , Give.requestExpression = "zero zero \8594"
                }
              >>= liftIO . expectGiveFailure "unparseable"
          liftIO $
            assertBool
              ( "unparseable: expected \"ParseError\" within "
                  <> show (errorMessage unparseable)
              )
              ("ParseError" `Text.isInfixOf` errorMessage unparseable)
    , testCase "give keeps an expression Agda did not change" $
        withFixtureSession warm "test/fixtures/GiveExpressions.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          plain <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 3
                , Give.requestExpression = "zero"
                }
              >>= liftIO . expectGiveOk "a plain expression"
          liftIO $ plain @?= GiveVerbatim False

          parenthesized <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 5
                , Give.requestExpression = "(zero)"
                }
              >>= liftIO . expectGiveOk "redundant parentheses"
          liftIO $ parenthesized @?= GiveVerbatim False

          underscore <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 6
                , Give.requestExpression = "_"
                }
              >>= liftIO . expectGiveOk "an underscore"
          liftIO $ underscore @?= GiveVerbatim False
    , testCase "give reports when the hole needs parentheses" $
        withFixtureSession warm "test/fixtures/GiveExpressions.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          giveAction <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 4
                , Give.requestExpression = "suc zero"
                }
              >>= liftIO . expectGiveOk "an application in argument position"
          liftIO $ giveAction @?= GiveVerbatim True
    ]

refineTests :: IO TCState -> TestTree
refineTests warm =
  testGroup
    "Refine"
    [ testCase "refine reports what it had to add to the expression" $
        withFixtureSession warm "test/fixtures/RefineExpressions.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          -- An expression that already fits needs no metavariables, so refine
          -- keeps the user's text just as give does. `give_gen` computes
          -- `literally` for `Refine` as well as `Give`.
          solving <-
            refine
              Refine.Request
                { Refine.requestGoalId = 0
                , Refine.requestExpression = "zero"
                }
              >>= liftIO . expectGiveOk "an expression that needs no arguments"
          liftIO $ solving @?= GiveVerbatim False

          twoHoles <-
            refine
              Refine.Request
                { Refine.requestGoalId = 1
                , Refine.requestExpression = "_+_"
                }
              >>= liftIO . expectGiveOk "an expression missing two arguments"
          liftIO $ twoHoles @?= GiveComputed "? + ?"
    , testCase "refine reports when no number of arguments would fit" $
        withFixtureSession warm "test/fixtures/RefineExpressions.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          -- `refine` appends up to ten metavariables before giving up
          -- (BasicOps.hs:267), and `tt` fits at no arity.
          unrefinable <-
            refine
              Refine.Request
                { Refine.requestGoalId = 2
                , Refine.requestExpression = "tt"
                }
              >>= liftIO . expectGiveFailure "an expression of the wrong type"
          liftIO $
            assertBool
              ( "unrefinable: expected \"CannotRefine\" within "
                  <> show (errorMessage unrefinable)
              )
              ("CannotRefine" `Text.isInfixOf` errorMessage unrefinable)
    ]

introTests :: IO TCState -> TestTree
introTests warm =
  testGroup
    "Intro"
    [ testCase "intro reports when the goal has no introduction form" $
        withFixtureSession warm "test/fixtures/IntroCandidates.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          introduced <-
            intro
              Intro.Request
                { Intro.requestPatternLambda = False
                , Intro.requestGoalId = 1
                }
              >>= liftIO . expectIntroError "a postulated type"
          liftIO $ introduced @?= IntroNotFound
    , testCase "intro reports every candidate when the choice is ambiguous" $
        withFixtureSession warm "test/fixtures/IntroCandidates.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          introduced <-
            intro
              Intro.Request
                { Intro.requestPatternLambda = False
                , Intro.requestGoalId = 2
                }
              >>= liftIO . expectIntroError "a type with two constructors"
          liftIO $ introduced @?= IntroAmbiguous ["false", "true"]
    , testCase "intro writes a function goal as a lambda" $
        withFixtureSession warm "test/fixtures/IntroCandidates.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          lambda <-
            intro
              Intro.Request
                { Intro.requestPatternLambda = False
                , Intro.requestGoalId = 3
                }
              >>= liftIO . expectIntroOk "a function goal"
          liftIO $ lambda @?= "λ x → ?"

          patternLambda <-
            intro
              Intro.Request
                { Intro.requestPatternLambda = True
                , Intro.requestGoalId = 4
                }
              >>= liftIO . expectIntroOk "a function goal, as a pattern lambda"
          liftIO $ patternLambda @?= "λ { x → ? }"
    , -- The candidate `introTactic` proposes is source text that still has to
      -- scope check where the hole is. The standard library's `⊥` is a record
      -- whose constructor `Data.Irrelevant.[_]` this fixture never imports, so
      -- intro proposes a name the give then rejects--reaching `IntroFailed`,
      -- which is distinct from finding no candidate at all.
      testCase "intro can propose a constructor that is not in scope" $
        withFixtureSession warm "test/fixtures/IntroCandidates.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          e <-
            intro
              Intro.Request
                { Intro.requestPatternLambda = False
                , Intro.requestGoalId = 0
                }
              >>= liftIO . expectIntroFailure "an unimported constructor"
          liftIO $
            assertBool
              ( "an unimported constructor: expected \"NotInScope\" within "
                  <> show (errorMessage e)
              )
              ("NotInScope" `Text.isInfixOf` errorMessage e)
    ]

elaborateGiveTests :: IO TCState -> TestTree
elaborateGiveTests warm =
  testGroup
    "ElaborateGive"
    [ testCase "the elaborated term is normalized at the requested level" $
        withFixtureSession warm "test/fixtures/ElaborateNormalization.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          -- Each level gets its own hole, since elaborating solves the one it
          -- is given.
          giveActions <-
            traverse
              ( \(goalId, normalization) ->
                  elaborateGive
                    ElaborateGive.Request
                      { ElaborateGive.requestNormalization = normalization
                      , ElaborateGive.requestGoalId = goalId
                      , ElaborateGive.requestExpression = "twice 2"
                      }
                    >>= liftIO . expectElaborated (show normalization)
              )
              (zip [0 ..] [AsIs, Instantiated, HeadNormal, Simplified, Normalised])
          liftIO $
            giveActions
              @?= [ "twice 2"
                  , "twice 2"
                  , "4"
                  , -- `simplify` collapses onto `AsIs` here, exactly as it does
                    -- for the goal types the metas report renders.
                    "twice 2"
                  , "4"
                  ]
    ]

scenarioTests :: IO TCState -> TestTree
scenarioTests warm =
  testGroup
    "Scenarios"
    [ -- Builds `+-assoc` the way a caller would. Inspect the goals, read a
      -- context, close the base case with `intro`, refine the inductive
      -- step, then fill what refine leaves remaining. Nothing is written to
      -- disk, so this is entirely session state.
      testCase "build a proof of addition associativity" $
        withFixtureSession warm "test/fixtures/ProofScenario.agda" $ \path -> do
          let request = Load.Request {Load.requestPath = path, Load.requestArguments = []}

          goals <-
            load request >>= liftIO . fmap metasReportGoals . expectLoadOk "load"
          liftIO $
            assertEqual
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
          liftIO $
            assertEqual
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
          liftIO $
            assertEqual
              "the base case sees only the arguments its pattern binds"
              (Right (GoalOfType "zero + n + p ≡ zero + (n + p)", ["n", "p"]))
              ( fmap
                  (\r -> (goalReportShape r, map contextEntryOriginalName (goalReportContext r)))
                  baseReport
              )

          -- The identity type has one constructor, so `intro` is unambiguous
          -- here.
          introduced <-
            intro
              Intro.Request
                { Intro.requestPatternLambda = False
                , Intro.requestGoalId = 0
                }
              >>= liftIO . expectIntroOk "intro closes the base case"
          liftIO $ introduced @?= "refl"

          refined <-
            refine
              Refine.Request
                { Refine.requestGoalId = 1
                , Refine.requestExpression = "cong suc"
                }
              >>= liftIO . expectGiveOk "refine leaves a hole for the recursive call"
          liftIO $ refined @?= GiveComputed "cong suc ?"

          -- Refine's new hole exists in the session immediately, without
          -- writing the splice to disk and reloading.
          recursiveCall <-
            goal
              Goal.Request
                { Goal.requestNormalization = AsIs
                , Goal.requestGoalId = 2
                }
          liftIO $
            assertEqual
              "the hole refine introduced is the recursive call"
              (Right (GoalOfType "m + n + p ≡ m + (n + p)"))
              (fmap goalReportShape recursiveCall)

          -- The file on disk never changed, so a reload throws all of that away
          -- and returns the original two goals.
          reloaded <-
            load request >>= liftIO . fmap metasReportGoals . expectLoadOk "reload"
          liftIO $
            assertEqual
              "reloading discards the session's progress"
              (map goalShape goals)
              (map goalShape reloaded)
    , -- Gives a real library proof back to the library's own statement of it.
      testCase "rebuild a standard library proof from its own text" $
        withFixtureSession warm "test/fixtures/StandardLibraryProof.agda" $ \path -> do
          goals <-
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . fmap metasReportGoals . expectLoadOk "load"
          -- The stated type is `Associative _++_`, a defined predicate, which
          -- does not survive into the goals: each hole shows the equation it
          -- unfolds to.
          liftIO $
            assertEqual
              "the clauses hold the unfolded associativity equations"
              [ GoalOfType "([] ++ ys) ++ zs ≡ [] ++ ys ++ zs"
              , GoalOfType "((x ∷ xs) ++ ys) ++ zs ≡ (x ∷ xs) ++ ys ++ zs"
              ]
              (map goalShape goals)

          -- A dependency-rich context: the clause's own binders, plus the
          -- generalized variables the module telescope introduces.
          stepReport <-
            goal
              Goal.Request
                { Goal.requestNormalization = AsIs
                , Goal.requestGoalId = 1
                }
          liftIO $
            assertEqual
              "the inductive step sees the generalized variables too"
              ( Right
                  ( ["a", "A", "x", "xs", "ys", "zs"]
                  , ["Level", "Set a", "A", "List A", "List A", "List A"]
                  )
              )
              ( fmap
                  ( \report ->
                      ( map contextEntryOriginalName (goalReportContext report)
                      , map contextEntryType (goalReportContext report)
                      )
                  )
                  stepReport
              )

          base <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 0
                , Give.requestExpression = "refl"
                }
              >>= liftIO . expectGiveOk "the standard library's base case"
          liftIO $ base @?= GiveVerbatim False

          inductiveStep <-
            give
              Give.Request
                { Give.requestForce = WithoutForce
                , Give.requestGoalId = 1
                , Give.requestExpression = "cong (x ∷_) (++-assoc xs ys zs)"
                }
              >>= liftIO . expectGiveOk "the standard library's inductive step"
          liftIO $ inductiveStep @?= GiveVerbatim False

          report <-
            metas Metas.Request {Metas.requestNormalization = AsIs}
              >>= liftIO . expectMetasOk "after both gives"
          liftIO $ report @?= MetasReport [] [] [] []
    , testCase "an elaboration can coincide with the text it elaborates" $
        withFixtureSession warm "test/fixtures/StandardLibraryProof.agda" $ \path -> do
          void $
            load Load.Request {Load.requestPath = path, Load.requestArguments = []}
              >>= liftIO . expectLoadOk "load"

          let body = "cong (x ∷_) (++-assoc xs ys zs)"
          elaborated <-
            elaborateGive
              ElaborateGive.Request
                { ElaborateGive.requestNormalization = Normalised
                , ElaborateGive.requestGoalId = 1
                , ElaborateGive.requestExpression = body
                }
              >>= liftIO . expectElaborated "the standard library's inductive step"
          liftIO $ elaborated @?= body
    ]

currentFile :: InteractionM (Maybe FilePath)
currentFile =
  gets $ \(InteractionState _ commandState _) ->
    filePath . currentFilePath <$> theCurrentFile commandState

expectLoadOk ::
  String ->
  Load.Response ->
  IO MetasReport
expectLoadOk _ (Load.ResponseOk report) = pure report
expectLoadOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectLoadError :: String -> Load.Response -> IO Error
expectLoadError _ (Load.ResponseError e) = pure e
expectLoadError label other =
  assertFailure $ label <> ": expected ResponseError, got " <> show other

expectMetasOk :: String -> Metas.Response -> IO MetasReport
expectMetasOk _ (Right report) = pure report
expectMetasOk label other =
  assertFailure $ label <> ": expected a metas report, got " <> show other

expectContextOk :: String -> Context.Response -> IO [ContextEntry]
expectContextOk _ (Right entries) = pure entries
expectContextOk label other =
  assertFailure $ label <> ": expected a context, got " <> show other

expectGiveOk :: String -> Give.Response -> IO GiveAction
expectGiveOk _ (Right action) = pure action
expectGiveOk label other =
  assertFailure $ label <> ": expected a give action, got " <> show other

-- Give and refine answer with a `GiveAction`, elaborate-give with the
-- elaborated text, but all three fail the same way.
expectGiveError :: (Show a) => String -> Either GiveError a -> IO GiveError
expectGiveError _ (Left e) = pure e
expectGiveError label other =
  assertFailure $ label <> ": expected a give error, got " <> show other

expectGiveFailure :: String -> Give.Response -> IO Error
expectGiveFailure label response = do
  e <- expectGiveError label response
  case e of
    GiveFailed e' -> pure e'
    other ->
      assertFailure $ label <> ": expected GiveFailed, got " <> show other

expectElaborated :: String -> ElaborateGive.Response -> IO Text
expectElaborated _ (Right elaborated) = pure elaborated
expectElaborated label other =
  assertFailure $
    label <> ": expected an elaborated expression, got " <> show other

expectIntroOk :: String -> Intro.Response -> IO Text
expectIntroOk _ (Right introduced) = pure introduced
expectIntroOk label other =
  assertFailure $ label <> ": expected an introduction form, got " <> show other

expectIntroError :: String -> Intro.Response -> IO IntroError
expectIntroError _ (Left e) = pure e
expectIntroError label other =
  assertFailure $ label <> ": expected an intro error, got " <> show other

expectIntroFailure :: String -> Intro.Response -> IO Error
expectIntroFailure label response = do
  e <- expectIntroError label response
  case e of
    IntroFailed e' -> pure e'
    other ->
      assertFailure $ label <> ": expected IntroFailed, got " <> show other

spanCoordinates :: Span -> ((Int, Int), (Int, Int))
spanCoordinates s =
  (coordinates (spanStart s), coordinates (spanEnd s))
 where
  coordinates p = (positionLine p, positionColumn p)

spanText :: Text -> Span -> Text
spanText source s =
  Text.take (end - start) (Text.drop start source)
 where
  start = positionOffset $ spanStart s
  end = positionOffset $ spanEnd s

warningMessage :: Warning -> Text
warningMessage (Warning (_, message)) = message

nonFatalErrorMessage :: NonFatalError -> Text
nonFatalErrorMessage (NonFatalError (_, message)) = message

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
  IO TCState -> FilePath -> (FilePath -> IO () -> InteractionM a) -> IO a
withStaleFixtureSession warm source k =
  withStagedFiles [source] $ \directory options -> do
    let staged = directory </> takeFileName source
    armed <- newIORef True
    state <- warmedSession warm options >>= touchWhileChecking armed staged
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

bogusGoalId :: InteractionId
bogusGoalId = 99
