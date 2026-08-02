{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Goal (tests) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.TypeChecking.Monad (TCState)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Corpus qualified as Corpus
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tool.Harness (withFixtureToolSession, withFixtureToolSessions)

import AgdaMCP.Interaction (
  ContextEntry (..),
  Goal (..),
  GoalReport (..),
  GoalShape (..),
 )
import AgdaMCP.Tools.Goal (
  Request (..),
  Response (..),
  renderResponse,
 )
import AgdaMCP.Tools.Goal qualified as Goal
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadId (..), LoadIdRefusal (..))
import AgdaMCP.Tools.MCP (normalizations, parseArguments, renderNormalization)
import Data.Foldable (for_)

tests :: IO TCState -> TestTree
tests warm =
  testGroup
    "Goal"
    [ parseArgumentsTests
    , renderNormalizationTests
    , renderResponseTests
    , sessionTests warm
    ]

parseArgumentsTests :: TestTree
parseArgumentsTests =
  testGroup
    "parseArguments"
    [ testCase "a call with no arguments at all is missing its load_id" $
        parseRequest Nothing
          @?= Left "Error in $: key \"load_id\" not found"
    , testCase "a load_id that does not start with L is refused" $
        parseRequest
          ( Just $
              Map.fromList
                [ ("load_id", Aeson.String "17")
                , ("goal", Aeson.Number 0)
                ]
          )
          @?= Left
            "Error in $['load_id']: expected a load_id from a load result, \
            \such as \"L17\", but got \"17\""
    , testCase "a call without a goal is refused" $
        parseRequest (Just $ Map.fromList [("load_id", Aeson.String "L1")])
          @?= Left "Error in $: key \"goal\" not found"
    , testCase "a goal that is not an integer is refused" $
        parseRequest
          ( Just $
              Map.fromList
                [ ("load_id", Aeson.String "L1")
                , ("goal", Aeson.String "?0")
                ]
          )
          @?= Left
            "Error in $.goal: parsing Int failed, expected Number, \
            \but encountered String"
    , testCase "an unknown normalization is refused" $
        parseRequest
          ( Just $
              Map.fromList
                [ ("load_id", Aeson.String "L1")
                , ("goal", Aeson.Number 0)
                , ("normalization", Aeson.String "very")
                ]
          )
          @?= Left
            "Error in $.normalization: expected one of asis, headnormal, \
            \instantiated, normalized, simplified"
    , testCase "normalization defaults to simplified" $
        parseRequest
          ( Just $
              Map.fromList
                [ ("load_id", Aeson.String "L3")
                , ("goal", Aeson.Number 0)
                ]
          )
          @?= Right (Request (LoadId 3) 0 Simplified)
    , testCase "all three arguments parse" $
        parseRequest
          ( Just $
              Map.fromList
                [ ("load_id", Aeson.String "L3")
                , ("goal", Aeson.Number 2)
                , ("normalization", Aeson.String "normalized")
                ]
          )
          @?= Right (Request (LoadId 3) 2 Normalised)
    ]
 where
  parseRequest :: Maybe (Map.Map Text Aeson.Value) -> Either Text Request
  parseRequest = parseArguments

renderNormalizationTests :: TestTree
renderNormalizationTests =
  testGroup
    "renderNormalization"
    [ testCase "every normalization renders as its own name" $
        for_ (Map.toList normalizations) $ \(name, normalization) ->
          renderNormalization normalization @?= name
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
            \is L2. Each load makes fresh goal ID assignments, so use the load \
            \ID and goal IDs the most recent load result. If you no longer \
            \have that result, load the file again."
    , testCase "an unknown goal id points back at the load result" $
        renderResponse (ResponseUnknownGoal 5)
          @?= Left
            "No goal ?5 in the current load. Check the goal IDs in the most \
            \recent load result."
    , testCase "a failed query reports Agda's error" $
        renderResponse (ResponseFailed Corpus.typeError)
          @?= Right
            ( rendered
                [ "Goal query failed:"
                , ""
                , "  /fixture/TypeError.agda:6.9-10: error: [UnequalTerms]"
                , "  Set !=< ℕ"
                , "  when checking that the expression ℕ has type ℕ"
                ]
            )
    , testCase "a failed query reports the warnings raised before the error" $
        renderResponse (ResponseFailed Corpus.warningThenError)
          @?= Right
            ( rendered
                [ "Goal query failed:"
                , ""
                , "  /fixture/WarningThenError.agda:11.9-10: error: [UnequalTerms]"
                , "  Set !=< ℕ"
                , "  when checking that the expression ℕ has type ℕ"
                , ""
                , "Warnings:"
                , "  /fixture/WarningThenError.agda:8.1-12: warning: -W[no]UnreachableClauses"
                , "  Unreachable clause"
                , "  when checking the definition of first"
                ]
            )
    , testCase "a goal with no context reports its type alone" $
        renderResponse (ResponseOk 0 Simplified emptyReport)
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "the header carries the goal id that was queried" $
        renderResponse (ResponseOk 3 Simplified emptyReport)
          @?= Right
            ( rendered
                [ "?3 (simplified)"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "the requested normalization is named beside the goal id" $
        renderResponse (ResponseOk 0 Normalised emptyReport)
          @?= Right
            ( rendered
                [ "?0 (normalized)"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "the context is reported innermost-first" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportShape = GoalOfType "(y + z) * x ≡ y * x + z * x"
                , goalReportContext =
                    [contextEntry "x" "ℕ", contextEntry "y" "ℕ", contextEntry "z" "ℕ"]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  z : ℕ"
                , "  y : ℕ"
                , "  x : ℕ"
                , "  ⊢ (y + z) * x ≡ y * x + z * x"
                ]
            )
    , testCase "a goal that is a sort says so" $
        renderResponse
          (ResponseOk 0 Simplified emptyReport {goalReportShape = GoalSort})
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ Sort"
                ]
            )
    , testCase "a goal type that spans lines keeps Agda's own indentation" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport {goalReportShape = GoalOfType "A\n  → B"}
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ A"
                , "    → B"
                ]
            )
    , testCase "context entries render as load renders them" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [(contextEntry "x" "ℕ") {contextEntryReifiedName = "x₁"}]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  x = x₁ : ℕ"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "boundary faces are reported in their own section" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportBoundary = ["i = i0 ⊢ x", "i = i1 ⊢ y"]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Boundary:"
                , "  i = i0 ⊢ x"
                , ""
                , "  i = i1 ⊢ y"
                ]
            )
    , testCase "constraints mentioning the goal are reported in their own section" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportConstraints = ["first constraint", "another constraint"]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Constraints:"
                , "  first constraint"
                , ""
                , "  another constraint"
                ]
            )
    , testCase "a constraint that spans lines keeps its own indentation" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportConstraints = ["top\n  nested"]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Constraints:"
                , "  top"
                , "    nested"
                ]
            )
    , testCase "the sections are reported in a fixed order" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext = [contextEntry "x" "ℕ"]
                , goalReportBoundary = ["i = i0 ⊢ x"]
                , goalReportConstraints = ["a constraint"]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  x : ℕ"
                , "  ⊢ ℕ"
                , ""
                , "Boundary:"
                , "  i = i0 ⊢ x"
                , ""
                , "Constraints:"
                , "  a constraint"
                ]
            )
    ]

sessionTests :: IO TCState -> TestTree
sessionTests warm =
  testGroup
    "sessions"
    [ testCase "a load_id from before a failed load is refused"
        $ withFixtureToolSessions
          warm
          ["test/fixtures/HoleNatural.agda", "test/fixtures/TypeError.agda"]
        $ \paths -> case paths of
          [holes, broken] -> do
            loaded <-
              Load.load Load.Request {Load.loadRequestPath = holes}
                >>= liftIO . expectLoadOk "the first load"
            liftIO $ Load.loadReportId loaded @?= LoadId 1
            failed <- Load.load Load.Request {Load.loadRequestPath = broken}
            liftIO $ case failed of
              Load.ResponseError _ -> pure ()
              other ->
                assertFailure $
                  "expected the second load to fail, got " <> show other
            refused <-
              Goal.goal
                Goal.Request
                  { goalRequestLoadId = LoadId 1
                  , goalRequestGoalId = 0
                  , goalRequestNormalization = Simplified
                  }
            liftIO $ refused @?= ResponseRefused NoCurrentLoad
          _ -> liftIO $ assertFailure "expected two staged fixtures"
    , testCase "a goal id that no load reported is unknown" $
        withFixtureToolSession warm "test/fixtures/HoleNatural.agda" $ \path -> do
          loaded <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          liftIO $
            map (goalId . fst) (Load.loadReportGoals loaded) @?= [0]
          response <-
            Goal.goal
              Goal.Request
                { goalRequestLoadId = LoadId 1
                , goalRequestGoalId = 5
                , goalRequestNormalization = Simplified
                }
          liftIO $ response @?= ResponseUnknownGoal 5
    , testCase "a load that succeeded with errors keeps its goals queryable" $
        withFixtureToolSession warm "test/fixtures/Constrained.agda" $ \path -> do
          loaded <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          liftIO $
            map
              (Corpus.normalizeNonFatalError path)
              (Load.loadReportNonFatalErrors loaded)
              @?= [Corpus.unsolvedConstraints]
          report <-
            Goal.goal
              Goal.Request
                { goalRequestLoadId = LoadId 1
                , goalRequestGoalId = 0
                , goalRequestNormalization = AsIs
                }
              >>= liftIO . expectGoalOk "the constrained goal"
          liftIO $ do
            goalReportShape report @?= GoalOfType "ℕ"
            case goalReportConstraints report of
              [equation, assignment] -> do
                assertBool
                  ("expected the postponed equation, got " <> show equation)
                  ("?0 + ?0 = 4 : ℕ" `Text.isInfixOf` equation)
                assertBool
                  ("expected the blocked assignment, got " <> show assignment)
                  ("p ?0" `Text.isInfixOf` assignment)
              other ->
                assertFailure $ "expected two constraints, got " <> show other
    ]

-- Helpers

rendered :: [Text] -> Text
rendered = Text.intercalate "\n"

emptyReport :: GoalReport
emptyReport =
  GoalReport
    { goalReportShape = GoalOfType "ℕ"
    , goalReportBoundary = []
    , goalReportContext = []
    , goalReportConstraints = []
    }

contextEntry :: Text -> Text -> ContextEntry
contextEntry name ty =
  ContextEntry
    { contextEntryOriginalName = name
    , contextEntryReifiedName = name
    , contextEntryOriginalInScope = True
    , contextEntryReifiedInScope = True
    , contextEntryType = ty
    , contextEntryLetValue = Nothing
    , contextEntryIsInstance = False
    , contextEntryCohesion = Nothing
    , contextEntryPolarity = Nothing
    , contextEntryErased = False
    , contextEntryRelevance = Nothing
    }

expectLoadOk :: String -> Load.Response -> IO Load.LoadReport
expectLoadOk _ (Load.ResponseOk report) = pure report
expectLoadOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectGoalOk :: String -> Goal.Response -> IO GoalReport
expectGoalOk _ (ResponseOk _ _ report) = pure report
expectGoalOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other
