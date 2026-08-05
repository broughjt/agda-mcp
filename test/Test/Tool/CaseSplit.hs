{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.CaseSplit (tests) where

import Agda.Syntax.Common (InteractionId)
import Agda.TypeChecking.Monad (TCState)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Functor (void)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Test.Corpus qualified as Corpus
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (
  assertBool,
  assertEqual,
  assertFailure,
  testCase,
  (@?=),
 )
import Test.Tool.Harness (withFixtureToolSession)

import AgdaMCP.Interaction (
  Error (..),
  Goal (..),
  GoalShape (..),
  Position (..),
  Span (..),
  Warning (..),
 )
import AgdaMCP.Interaction.MakeCase (MakeCaseVariant (..))
import AgdaMCP.Tools.CaseSplit (
  ClauseLayout (..),
  Edit (..),
  Outcome (..),
  Request (..),
  Response (..),
  caseSplit,
  clauseLayout,
  layoutClauses,
  renderResponse,
 )
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadId (..), LoadIdRefusal (..))
import AgdaMCP.Tools.MCP (parseArguments)
import AgdaMCP.Tools.State (ToolM)

tests :: IO TCState -> TestTree
tests warm =
  testGroup
    "CaseSplit"
    [ parseArgumentsTests
    , clauseLayoutTests
    , layoutClausesTests
    , renderResponseTests
    , sessionTests warm
    ]

parseArgumentsTests :: TestTree
parseArgumentsTests =
  testGroup
    "parseArguments"
    [ testCase "variables must be supplied, since silence would introduce" $
        parseRequest
          ( Just $
              Map.fromList
                [("load_id", Aeson.String "L1"), ("goal", Aeson.Number 0)]
          )
          @?= Left "Error in $: key \"variables\" not found"
    , testCase "variables must be an array" $
        parseRequest (argumentsValue (Aeson.String "n"))
          @?= Left
            "Error in $.variables: parsing variables failed, expected Array, \
            \but encountered String"
    , testCase "the ellipsis marker is refused" $
        parseRequest (arguments ["."])
          @?= Left
            "Error in $.variables[0]: expected a pattern variable name, but got \
            \\".\", which Agda reads as the ellipsis of a with-clause rather \
            \than as a variable"
    , testCase "a blank name is refused" $
        parseRequest (arguments [""])
          @?= Left
            "Error in $.variables[0]: expected a pattern variable name, but \
            \got \"\""
    , testCase "a whitespace-only name is refused" $
        parseRequest (arguments ["  "])
          @?= Left
            "Error in $.variables[0]: expected a pattern variable name, but \
            \got \"  \""
    , testCase "a padded name is refused rather than trimmed" $
        parseRequest (arguments [" n"])
          @?= Left
            "Error in $.variables[0]: expected a pattern variable name with no \
            \surrounding whitespace, but got \" n\""
    , testCase "a name carrying whitespace is reported as several names" $
        parseRequest (arguments ["suc n"])
          @?= Left
            "Error in $.variables[0]: expected a single pattern variable name, \
            \but got \"suc n\", which is 2 names; send each as its own entry"
    , testCase "the ellipsis marker is refused anywhere in the list" $
        parseRequest (arguments ["n", "."])
          @?= Left
            "Error in $.variables[1]: expected a pattern variable name, but got \
            \\".\", which Agda reads as the ellipsis of a with-clause rather \
            \than as a variable"
    , testCase "a plain name is accepted" $
        parseRequest (arguments ["n"])
          @?= Right (Request (LoadId 1) 0 ["n"])
    , testCase "several names are accepted in order" $
        parseRequest (arguments ["x", "y"])
          @?= Right (Request (LoadId 1) 0 ["x", "y"])
    , testCase "an empty list is accepted, and selects introduction" $
        parseRequest (arguments [])
          @?= Right (Request (LoadId 1) 0 [])
    ]
 where
  parseRequest :: Maybe (Map.Map Text Aeson.Value) -> Either Text Request
  parseRequest = parseArguments

  arguments :: [Text] -> Maybe (Map.Map Text Aeson.Value)
  arguments = argumentsValue . Aeson.toJSON

  argumentsValue :: Aeson.Value -> Maybe (Map.Map Text Aeson.Value)
  argumentsValue variables =
    Just $
      Map.fromList
        [ ("load_id", Aeson.String "L1")
        , ("goal", Aeson.Number 0)
        , ("variables", variables)
        ]

clauseLayoutTests :: TestTree
clauseLayoutTests =
  testGroup
    "clauseLayout"
    [ testCase "a clause starting its line takes one clause per line" $
        clauseLayout
          (source ["double : ℕ → ℕ", "double n = ?", ""])
          (Span (Position 15 2 1) (Position 27 2 13))
          @?= OnePerLine
    , testCase "a clause behind an opening brace stays inline" $
        clauseLayout
          (source ["extended = λ { n → ? }", ""])
          (Span (Position 15 1 16) (Position 20 1 21))
          @?= Inline
    , testCase "a lambda-where clause is indented but still starts its line" $
        clauseLayout
          (source ["extendedWhere = λ where", "  n → ?", ""])
          (Span (Position 26 2 3) (Position 31 2 8))
          @?= OnePerLine
    , testCase "a clause inside a where block starts its line" $
        clauseLayout
          (source ["f = g", "  where", "    g : ℕ", "    g = ?", ""])
          (Span (Position 28 4 5) (Position 33 4 10))
          @?= OnePerLine
    ]

layoutClausesTests :: TestTree
layoutClausesTests =
  testGroup
    "layoutClauses"
    [ testCase "one clause per line joins with newlines" $
        layoutClauses OnePerLine ["double zero = ?", "double (suc n) = ?"]
          @?= "double zero = ?\ndouble (suc n) = ?"
    , testCase "inline joins with Agda's clause separator" $
        layoutClauses Inline ["zero → ?", "(suc n) → ?"]
          @?= "zero → ? ; (suc n) → ?"
    , testCase "a single clause is unchanged, one per line" $
        layoutClauses OnePerLine ["introduce x = ?"] @?= "introduce x = ?"
    , testCase "a single clause is unchanged, inline" $
        layoutClauses Inline ["n → ?"] @?= "n → ?"
    ]

renderResponseTests :: TestTree
renderResponseTests =
  testGroup
    "renderResponse"
    [ testCase "a refusal when nothing is loaded says to load first" $
        renderResponse (ResponseRefused NoCurrentLoad)
          @?= Left
            "No load is current. Either no file has been loaded yet, or the \
            \most recent load failed. Load the file, then use the load ID and \
            \goal IDs from that load result."
    , testCase "a stale load_id names the current load" $
        renderResponse (ResponseRefused $ StaleLoadId $ LoadId 2)
          @?= Left
            "The supplied load ID is from an earlier load. The current load ID \
            \is L2. Each load makes fresh goal ID assignments, so use the most \
            \recently issued load ID and goal IDs. If you no longer have them, \
            \load the file again."
    , testCase "a split reports the clauses it wrote" $
        completed 0 (OutcomeApplied distribEdit)
          @?= Right
            ( rendered $
                [ "Replaced the clause at ?0 with 2 clauses:"
                , "  *-distribʳ-+ x zero z = ?"
                , "  *-distribʳ-+ x (suc y) z = ?"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "introducing an argument reports one clause" $
        completed
          0
          ( OutcomeApplied
              distribEdit {editClauses = ["*-distribʳ-+ x y z = ?"]}
          )
          @?= Right
            ( rendered $
                [ "Replaced the clause at ?0 with 1 clause:"
                , "  *-distribʳ-+ x y z = ?"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "an extended lambda's clauses are written on one line" $
        completed
          1
          ( OutcomeApplied
              distribEdit
                { editVariant = MakeCaseExtendedLambda
                , editLayout = Inline
                , editClauses = ["zero → ?", "(suc n) → ?"]
                }
          )
          @?= Right
            ( rendered $
                [ "Replaced the clause at ?1 with 2 clauses:"
                , "  zero → ? ; (suc n) → ?"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "a collapsed where block is called out under the clauses" $
        completed
          0
          ( OutcomeApplied
              distribEdit
                { editClauses = ["withWhere zero = ?", "withWhere (suc n) = ?"]
                , editCollapsesWhere = True
                }
          )
          @?= Right
            ( rendered $
                [ "Replaced the clause at ?0 with 2 clauses:"
                , "  withWhere zero = ?"
                , "  withWhere (suc n) = ?"
                , ""
                , "Warning: the `where` block that followed the clause you \
                  \split now belongs to the last of the new clauses alone, so \
                  \the earlier clauses cannot see its bindings. If they use \
                  \those bindings, the reload below reports the errors. Lift \
                  \the bindings to the enclosing module or give each clause \
                  \its own `where` block."
                , ""
                ]
                  <> reloadLines
            )
    , testCase "an unknown goal id is refused without writing" $
        completed 7 OutcomeUnknownGoal
          @?= Right
            ( rendered $
                [ "Cannot split ?7. No edits were written."
                , ""
                , "  There is no such goal in the current load. Check the goal \
                  \IDs in the most recent result."
                , ""
                ]
                  <> reloadLines
            )
    , testCase "a failed split reports Agda's error at its file position" $
        completed 0 (OutcomeFailed Corpus.caseSplitError)
          @?= Right
            ( rendered $
                [ "Cannot split ?0. No edits were written."
                , ""
                , "  /fixture/MakeCase.agda:9.12-13: error: \
                  \[Interaction.CaseSplitError]"
                , "  Unbound variable nope"
                , "  when checking that the expression ? has type ℕ"
                , ""
                ]
                  <> reloadLines
            )
    , -- No observed CaseSplitError carries warnings, so the payload here is
      -- artificial and the case is about where the section lands.
      testCase "a failure's warnings follow as their own section" $
        completed
          0
          ( OutcomeFailed
              Corpus.caseSplitError
                { errorMessage = "it cannot be split"
                , errorWarnings =
                    [Warning (Nothing, "a warning raised on the way")]
                }
          )
          @?= Right
            ( rendered $
                [ "Cannot split ?0. No edits were written."
                , ""
                , "  it cannot be split"
                , ""
                , "Warnings:"
                , "  a warning raised on the way"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "a changed file is refused before anything is written" $
        completed 0 OutcomeFileChanged
          @?= Right
            ( rendered $
                [ "The file on disk no longer matches the source Agda checked, \
                  \so no edits were written. It has been reloaded below; goal \
                  \IDs from the earlier load are no longer valid."
                , ""
                ]
                  <> reloadLines
            )
    , testCase "an unreadable file reports why it could not be read" $
        completed 0 (OutcomeSourceUnreadable "Example.agda: openFile: does not exist")
          @?= Right
            ( rendered $
                [ "Could not read the file to check it still matches the source \
                  \Agda checked, so no edits were written. It has been reloaded \
                  \below, and goal IDs from the earlier load are no longer valid."
                , ""
                , "  Example.agda: openFile: does not exist"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "a failed write reports that the file is unchanged" $
        completed 0 (OutcomeWriteFailed "Example.agda: openFile: permission denied")
          @?= Right
            ( rendered $
                [ "Could not write the file, so it was not modified. It has been \
                  \reloaded below, and goal IDs from the earlier load are no \
                  \longer valid."
                , ""
                , "  Example.agda: openFile: permission denied"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "the reload is rendered exactly as the load tool renders it" $
        renderResponse
          (ResponseCompleted 0 (OutcomeApplied distribEdit) reloadWithGoal)
          @?= Right
            ( rendered
                [ "Replaced the clause at ?0 with 2 clauses:"
                , "  *-distribʳ-+ x zero z = ?"
                , "  *-distribʳ-+ x (suc y) z = ?"
                , ""
                , "Load succeeded: 1 goal"
                , "Load ID: L5"
                , "File: /tmp/Example.agda"
                , ""
                , "?0 at 14:16-17"
                , "  ⊢ ℕ"
                ]
            )
    , -- A split is the first tool that can write a file which then fails to
      -- load. Nothing extra is said about it; load's error render carries it.
      testCase "a reload that the split broke is reported as load reports it" $
        renderResponse
          ( ResponseCompleted
              0
              (OutcomeApplied distribEdit {editCollapsesWhere = True})
              (Load.ResponseError Corpus.typeError)
          )
          @?= Right
            ( rendered
                [ "Replaced the clause at ?0 with 2 clauses:"
                , "  *-distribʳ-+ x zero z = ?"
                , "  *-distribʳ-+ x (suc y) z = ?"
                , ""
                , "Warning: the `where` block that followed the clause you \
                  \split now belongs to the last of the new clauses alone, so \
                  \the earlier clauses cannot see its bindings. If they use \
                  \those bindings, the reload below reports the errors. Lift \
                  \the bindings to the enclosing module or give each clause \
                  \its own `where` block."
                , ""
                , "Load failed:"
                , ""
                , "  /fixture/TypeError.agda:6.9-10: error: [UnequalTerms]"
                , "  Set !=< ℕ"
                , "  when checking that the expression ℕ has type ℕ"
                ]
            )
    ]

sessionTests :: IO TCState -> TestTree
sessionTests warm =
  testGroup
    "sessions"
    [ testCase "an extended lambda is spliced inside its braces" $
        withFixtureToolSession warm "test/fixtures/BraceLambdaSplit.agda" $ \path -> do
          void $ expectLoaded (LoadId 1) path
          (outcome, resync) <-
            caseSplit (Request (LoadId 1) 0 ["n"])
              >>= liftIO . expectCompleted 0 "a split inside a brace lambda"
          report <- liftIO $ expectLoadOk "the resync" resync
          liftIO $ do
            edit <- expectApplied outcome
            editVariant edit @?= MakeCaseExtendedLambda
            editLayout edit @?= Inline
            editClauses edit @?= ["zero → ?", "(suc n) → ?"]
            editCollapsesWhere edit @?= False
            contentsShouldBe path braceLambdaSplit
            Load.loadReportId report @?= LoadId 2
            goalShapes report @?= replicate 2 (GoalOfType "ℕ")
    , -- " ; " also parses here, so the layout is a choice rather than a
      -- requirement; it is Emacs's, and it is what this pins.
      testCase "a lambda-where block is spliced one clause per line" $
        withFixtureToolSession warm "test/fixtures/LambdaWhereSplit.agda" $ \path -> do
          void $ expectLoaded (LoadId 1) path
          (outcome, resync) <-
            caseSplit (Request (LoadId 1) 0 ["n"])
              >>= liftIO . expectCompleted 0 "a split inside a lambda-where"
          report <- liftIO $ expectLoadOk "the resync" resync
          liftIO $ do
            edit <- expectApplied outcome
            editVariant edit @?= MakeCaseExtendedLambda
            editLayout edit @?= OnePerLine
            editClauses edit @?= ["zero → ?", "(suc n) → ?"]
            contentsShouldBe path lambdaWhereSplit
            Load.loadReportId report @?= LoadId 2
            goalShapes report @?= replicate 2 (GoalOfType "ℕ")
    , testCase "splitting a clause with a where block breaks the file" $
        withFixtureToolSession warm "test/fixtures/WhereCollapse.agda" $ \path -> do
          void $ expectLoaded (LoadId 1) path
          (outcome, resync) <-
            caseSplit (Request (LoadId 1) 0 ["n"])
              >>= liftIO . expectCompleted 0 "a split over a where block"
          liftIO $ do
            edit <- expectApplied outcome
            editVariant edit @?= MakeCaseFunction
            editCollapsesWhere edit @?= True
            editClauses edit
              @?= [ "withWhere zero = ? + helper"
                  , "withWhere (suc n) = ? + helper"
                  ]
            contentsShouldBe path whereCollapseSplit
            case resync of
              Load.ResponseError e ->
                assertBool
                  ("expected helper out of scope, got " <> show (errorMessage e))
                  ("Not in scope" `Text.isInfixOf` errorMessage e)
              other -> assertFailure $ "expected a failed reload, got " <> show other
    , testCase "a file edited on disk since the load is refused unwritten" $
        withFixtureToolSession warm "test/fixtures/MakeCase.agda" $ \path -> do
          void $ expectLoaded (LoadId 1) path
          liftIO $ Text.IO.appendFile path onDiskEdit
          contents <- liftIO $ Text.IO.readFile path
          (outcome, resync) <-
            caseSplit (Request (LoadId 1) 0 ["n"])
              >>= liftIO . expectCompleted 0 "a split against an edited file"
          report <- liftIO $ expectLoadOk "the resync" resync
          liftIO $ do
            outcome @?= OutcomeFileChanged
            contentsShouldBe path contents
            Load.loadReportId report @?= LoadId 2
    , testCase "a goal id that is not in the load is reloaded and issues the next id" $
        withFixtureToolSession warm "test/fixtures/MakeCase.agda" $ \path -> do
          void $ expectLoaded (LoadId 1) path
          contents <- liftIO $ Text.IO.readFile path
          (outcome, resync) <-
            caseSplit (Request (LoadId 1) 99 ["n"])
              >>= liftIO . expectCompleted 99 "a split at a goal that does not exist"
          report <- liftIO $ expectLoadOk "the resync" resync
          liftIO $ do
            outcome @?= OutcomeUnknownGoal
            contentsShouldBe path contents
            Load.loadReportId report @?= LoadId 2
    , testCase "an unbound variable is reported and writes nothing" $
        withFixtureToolSession warm "test/fixtures/MakeCase.agda" $ \path -> do
          void $ expectLoaded (LoadId 1) path
          contents <- liftIO $ Text.IO.readFile path
          (outcome, resync) <-
            caseSplit (Request (LoadId 1) 0 ["nope"])
              >>= liftIO . expectCompleted 0 "a split on a name that is not bound"
          report <- liftIO $ expectLoadOk "the resync" resync
          liftIO $ do
            case outcome of
              OutcomeFailed e ->
                Corpus.normalizeError path e @?= Corpus.caseSplitError
              other -> assertFailure $ "unexpected outcome: " <> show other
            contentsShouldBe path contents
            Load.loadReportId report @?= LoadId 2
    , testCase "a stale load id is refused before anything runs" $
        withFixtureToolSession warm "test/fixtures/MakeCase.agda" $ \path -> do
          void $ expectLoaded (LoadId 1) path
          contents <- liftIO $ Text.IO.readFile path
          refused <- caseSplit (Request (LoadId 7) 0 ["n"])
          liftIO $ do
            refused @?= ResponseRefused (StaleLoadId $ LoadId 1)
            contentsShouldBe path contents
    ]

-- Helpers

expectLoaded :: LoadId -> FilePath -> ToolM Load.LoadReport
expectLoaded loadId path = do
  report <-
    Load.load Load.Request {Load.loadRequestPath = path}
      >>= liftIO . expectLoadOk "load"
  liftIO $ Load.loadReportId report @?= loadId
  pure report

expectLoadOk :: String -> Load.Response -> IO Load.LoadReport
expectLoadOk _ (Load.ResponseOk report) = pure report
expectLoadOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectCompleted ::
  InteractionId -> String -> Response -> IO (Outcome, Load.Response)
expectCompleted goalId label (ResponseCompleted reported outcome resync) = do
  assertEqual
    (label <> ": the response names the goal it was asked about")
    goalId
    reported
  pure (outcome, resync)
expectCompleted _ label other =
  assertFailure $ label <> ": expected ResponseCompleted, got " <> show other

expectApplied :: Outcome -> IO Edit
expectApplied (OutcomeApplied edit) = pure edit
expectApplied other = assertFailure $ "expected a split, got " <> show other

goalShapes :: Load.LoadReport -> [GoalShape]
goalShapes = map (goalShape . fst) . Load.loadReportGoals

contentsShouldBe :: FilePath -> Text -> IO ()
contentsShouldBe path expected = do
  contents <- Text.IO.readFile path
  contents @?= expected

onDiskEdit :: Text
onDiskEdit = "\n-- edited on disk\n"

braceLambdaSplit :: Text
braceLambdaSplit =
  Text.unlines
    [ "module BraceLambdaSplit where"
    , ""
    , "open import Data.Nat using (ℕ; zero; suc)"
    , ""
    , "extended : ℕ → ℕ"
    , "extended = λ { zero → ? ; (suc n) → ? }"
    ]

lambdaWhereSplit :: Text
lambdaWhereSplit =
  Text.unlines
    [ "module LambdaWhereSplit where"
    , ""
    , "open import Data.Nat using (ℕ; zero; suc)"
    , ""
    , "extendedWhere : ℕ → ℕ"
    , "extendedWhere = λ where"
    , "  zero → ?"
    , "  (suc n) → ?"
    ]

whereCollapseSplit :: Text
whereCollapseSplit =
  Text.unlines
    [ "module WhereCollapse where"
    , ""
    , "open import Data.Nat using (ℕ; zero; suc; _+_)"
    , ""
    , "withWhere : ℕ → ℕ"
    , "withWhere zero = ? + helper"
    , "withWhere (suc n) = ? + helper"
    , "  where"
    , "    helper : ℕ"
    , "    helper = zero"
    ]

source :: [Text] -> Text
source = Text.intercalate "\n"

rendered :: [Text] -> Text
rendered = Text.intercalate "\n"

completed :: InteractionId -> Outcome -> Either Text Text
completed goalId outcome =
  renderResponse (ResponseCompleted goalId outcome reload)

reload :: Load.Response
reload = Load.ResponseOk reloadReport

reloadLines :: [Text]
reloadLines =
  [ "Load succeeded: no goals"
  , "Load ID: L5"
  , "File: /tmp/Example.agda"
  ]

reloadReport :: Load.LoadReport
reloadReport =
  Load.LoadReport
    { Load.loadReportId = LoadId 5
    , Load.loadReportPath = "/tmp/Example.agda"
    , Load.loadReportGoals = []
    , Load.loadReportHiddenMetavariables = []
    , Load.loadReportWarnings = []
    , Load.loadReportNonFatalErrors = []
    }

reloadWithGoal :: Load.Response
reloadWithGoal =
  Load.ResponseOk
    reloadReport
      { Load.loadReportGoals =
          [
            ( Goal
                { goalId = 0
                , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                , goalShape = GoalOfType "ℕ"
                }
            , []
            )
          ]
      }

distribEdit :: Edit
distribEdit =
  Edit
    { editSpan = Span (Position 100 9 1) (Position 128 9 29)
    , editVariant = MakeCaseFunction
    , editLayout = OnePerLine
    , editClauses =
        [ "*-distribʳ-+ x zero z = ?"
        , "*-distribʳ-+ x (suc y) z = ?"
        ]
    , editCollapsesWhere = False
    }
