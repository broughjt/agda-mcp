{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Goal (tests) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.TypeChecking.Monad (TCState)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Directory (removeFile)
import Test.Corpus qualified as Corpus
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tool.Harness (
  describedKinds,
  loadId,
  withFixtureToolSession,
  withFixtureToolSessions,
 )

import AgdaMCP.Interaction (
  ContextEntry (..),
  Error (..),
  Goal (..),
  GoalReport (..),
  GoalShape (..),
 )
import AgdaMCP.Tools.Goal (
  Request (..),
  Response (..),
  goalTool,
  renderResponse,
 )
import AgdaMCP.Tools.Goal qualified as Goal
import AgdaMCP.Tools.Load (LoadRefusal (..))
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadId, LoadIdRefusal (..))
import AgdaMCP.Tools.Source (SourceRefusal (..))
import AgdaMCP.Tools.Tool (normalizations, parseArguments, renderNormalization)
import Data.Foldable (for_)

tests :: IO TCState -> TestTree
tests warm =
  testGroup
    "Goal"
    [ parseArgumentsTests
    , renderNormalizationTests
    , renderResponseTests
    , toJSONTests
    , sessionTests warm
    ]

parseArgumentsTests :: TestTree
parseArgumentsTests =
  testGroup
    "parseArguments"
    [ testCase "a call with no arguments at all is missing its load_id" $
        parseRequest Map.empty
          @?= Left "Error in $: key \"load_id\" not found"
    , testCase "a load_id that does not start with L is refused" $
        parseRequest
          ( Map.fromList
              [ ("load_id", Aeson.String "17")
              , ("goal", Aeson.Number 0)
              ]
          )
          @?= Left
            "Error in $['load_id']: expected a load_id from the most recent \
            \result, such as \"L17-k7x2\", but got \"17\""
    , testCase "a call without a goal is refused" $
        parseRequest (Map.fromList [("load_id", Aeson.String "L1-k7x2")])
          @?= Left "Error in $: key \"goal\" not found"
    , testCase "a goal that is not an integer is refused" $
        parseRequest
          ( Map.fromList
              [ ("load_id", Aeson.String "L1-k7x2")
              , ("goal", Aeson.String "?0")
              ]
          )
          @?= Left
            "Error in $.goal: parsing Int failed, expected Number, \
            \but encountered String"
    , testCase "an unknown normalization is refused" $
        parseRequest
          ( Map.fromList
              [ ("load_id", Aeson.String "L1-k7x2")
              , ("goal", Aeson.Number 0)
              , ("normalization", Aeson.String "very")
              ]
          )
          @?= Left
            "Error in $.normalization: expected one of asis, headnormal, \
            \instantiated, normalized, simplified"
    , testCase "normalization defaults to simplified" $
        parseRequest
          ( Map.fromList
              [ ("load_id", Aeson.String "L3-k7x2")
              , ("goal", Aeson.Number 0)
              ]
          )
          @?= Right (Request (loadId 3) 0 Simplified)
    , testCase "all three arguments parse" $
        parseRequest
          ( Map.fromList
              [ ("load_id", Aeson.String "L3-k7x2")
              , ("goal", Aeson.Number 2)
              , ("normalization", Aeson.String "normalized")
              ]
          )
          @?= Right (Request (loadId 3) 2 Normalised)
    ]
 where
  parseRequest :: Map.Map Text Aeson.Value -> Either Text Request
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
        renderResponse (ResponseRefused (RefusedLoadId NoCurrentLoad))
          @?= Left
            "No load is current. Either no file has been loaded yet, or the \
            \most recent load failed. Load the file, then use the load ID and \
            \goal IDs from that load result."
    , testCase "a stale load_id names the current load" $
        renderResponse (ResponseRefused $ RefusedLoadId $ StaleLoadId $ loadId 2)
          @?= Left
            "The supplied load ID is from an earlier load. The current load ID \
            \is L2-k7x2. Each load makes fresh goal ID assignments, so use the most \
            \recently issued load ID and goal IDs. If you no longer have them, \
            \load the file again."
    , testCase "a file that changed on disk says to load it again" $
        renderResponse (ResponseRefused $ RefusedSource RefusalChanged)
          @?= Left
            "The file on disk no longer matches the source Agda checked. Load \
            \the file again, then use the load ID and goal IDs from that load \
            \result."
    , testCase "a file that could not be read reports why" $
        renderResponse
          (ResponseRefused $ RefusedSource $ RefusalUnreadable "no such file")
          @?= Left
            ( rendered
                [ "Could not read the file to check it still matches the \
                  \source Agda checked. Load the file again once it is \
                  \readable, then use the load ID and goal IDs from that load \
                  \result."
                , ""
                , "  no such file"
                ]
            )
    , testCase "an unknown goal id points back at the load result" $
        renderResponse (ResponseUnknownGoal 5)
          @?= Left
            "No goal ?5 in the current load. Check the goal IDs in the most \
            \recent result."
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
                , "      → B"
                ]
            )
    , testCase "a goal type that spans lines continues under its start" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport {goalReportShape = GoalOfType "A\n→ B"}
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ A"
                , "    → B"
                ]
            )
    , testCase "a shadowed binding is reported under both of its names" $
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
    , testCase "a shadowed binding that has left scope says so" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [ (contextEntry "x" "ℕ")
                        { contextEntryReifiedName = "x₁"
                        , contextEntryReifiedInScope = False
                        }
                    ]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  x = x₁ : ℕ (not in scope)"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "a binder out of scope is reported under its reified name" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [ (contextEntry "x" "ℕ")
                        { contextEntryReifiedName = "x₁"
                        , contextEntryOriginalInScope = False
                        }
                    ]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  x₁ : ℕ"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "two anonymous binders are told apart by their reified names" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [ (contextEntry "x" "ℕ")
                        { contextEntryOriginalInScope = False
                        , contextEntryReifiedInScope = False
                        }
                    , (contextEntry "x" "ℕ")
                        { contextEntryReifiedName = "x₁"
                        , contextEntryOriginalInScope = False
                        , contextEntryReifiedInScope = False
                        }
                    ]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  x₁ : ℕ (not in scope)"
                , "  x : ℕ (not in scope)"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "a let binding is reported with its value" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [ contextEntry "m" "ℕ"
                    , (contextEntry "doubled" "ℕ")
                        { contextEntryLetValue = Just "m + m"
                        }
                    ]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  doubled : ℕ"
                , "  doubled = m + m"
                , "  m : ℕ"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "binder attributes are reported beside the binding" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [ (contextEntry "eq" "n ≡ n") {contextEntryIsInstance = True}
                    , (contextEntry "n" "ℕ") {contextEntryErased = True}
                    , (contextEntry "i" "ℕ") {contextEntryRelevance = Just "irrelevant"}
                    , (contextEntry "c" "ℕ") {contextEntryCohesion = Just "@♭"}
                    , (contextEntry "p" "ℕ") {contextEntryPolarity = Just "unused"}
                    ]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  p : ℕ (unused)"
                , "  @♭ c : ℕ"
                , "  i : ℕ (irrelevant)"
                , "  n : ℕ (erased)"
                , "  eq : n ≡ n (instance)"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "a binding carrying several attributes lists them in one group" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [ (contextEntry "x" "A")
                        { contextEntryReifiedName = "x₁"
                        , contextEntryReifiedInScope = False
                        , contextEntryIsInstance = True
                        , contextEntryCohesion = Just "@♭"
                        , contextEntryPolarity = Just "positive"
                        , contextEntryErased = True
                        , contextEntryRelevance = Just "irrelevant"
                        }
                    ]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  @♭ x = x₁ : A (not in scope, erased, irrelevant, positive, instance)"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "a context type that spans lines keeps Agda's own indentation" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [(contextEntry "f" "A\n  → B") {contextEntryIsInstance = True}]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  f : A"
                , "        → B (instance)"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "a context type that spans lines continues under its start" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport {goalReportContext = [contextEntry "f" "A\n→ B"]}
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  f : A"
                , "      → B"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "a wrapped type continues under the name and its cohesion" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [(contextEntry "c" "A\n→ B") {contextEntryCohesion = Just "@♭"}]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  @♭ c : A"
                , "         → B"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "a wrapped type continues under both names of a shadowed binding" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [(contextEntry "x" "A\n→ B") {contextEntryReifiedName = "x₁"}]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  x = x₁ : A"
                , "           → B"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "binder attributes follow the last line of a wrapped type" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [(contextEntry "f" "A\n→ B") {contextEntryIsInstance = True}]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  f : A"
                , "      → B (instance)"
                , "  ⊢ ℕ"
                ]
            )
    , testCase "a let value that spans lines continues under its start" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              emptyReport
                { goalReportContext =
                    [(contextEntry "doubled" "ℕ") {contextEntryLetValue = Just "m\n+ m"}]
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  doubled : ℕ"
                , "  doubled = m"
                , "            + m"
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

toJSONTests :: TestTree
toJSONTests =
  testGroup
    "toJSON"
    [ testCase "a refusal is its own kind, with the prose as its message" $
        Aeson.toJSON (ResponseRefused $ RefusedLoadId $ StaleLoadId $ loadId 2)
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "stale_load_id"
            , "current_load_id" Aeson..= Aeson.String "L2-k7x2"
            , "message"
                Aeson..= Aeson.String
                  "The supplied load ID is from an earlier load. The current load ID \
                  \is L2-k7x2. Each load makes fresh goal ID assignments, so use the most \
                  \recently issued load ID and goal IDs. If you no longer have them, \
                  \load the file again."
            ]
    , testCase "an unknown goal" $
        Aeson.toJSON (ResponseUnknownGoal 5)
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "unknown_goal"
            , "goal" Aeson..= (5 :: Int)
            , "message"
                Aeson..= Aeson.String
                  "No goal ?5 in the current load. Check the goal IDs in the most \
                  \recent result."
            ]
    , testCase "a failed query carries Agda's error" $
        Aeson.toJSON (ResponseFailed (plainError "it failed"))
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "failed"
            , "error"
                Aeson..= Aeson.object
                  [ "message" Aeson..= Aeson.String "it failed"
                  , "location" Aeson..= Aeson.Null
                  , "warnings" Aeson..= ([] :: [Aeson.Value])
                  ]
            ]
    , testCase "a goal carries every section, the context in Agda's order" $
        Aeson.toJSON
          ( ResponseOk
              3
              Normalised
              emptyReport
                { goalReportContext = [contextEntry "x" "ℕ", contextEntry "y" "ℕ"]
                , goalReportBoundary = ["i = i0 ⊢ x"]
                , goalReportConstraints = ["a constraint"]
                }
          )
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "goal"
            , "goal" Aeson..= (3 :: Int)
            , "normalization" Aeson..= Aeson.String "normalized"
            , "shape"
                Aeson..= Aeson.object
                  [ "kind" Aeson..= Aeson.String "of_type"
                  , "type" Aeson..= Aeson.String "ℕ"
                  ]
            , "context" Aeson..= [entryValue "x" "ℕ", entryValue "y" "ℕ"]
            , "boundary" Aeson..= [Aeson.String "i = i0 ⊢ x"]
            , "constraints" Aeson..= [Aeson.String "a constraint"]
            ]
    , testCase "every kind is described" $
        describedKinds
          goalTool
          renderResponse
          [ ResponseRefused $ RefusedLoadId $ StaleLoadId $ loadId 2
          , ResponseRefused $ RefusedSource RefusalChanged
          , ResponseUnknownGoal 5
          , ResponseFailed (plainError "it failed")
          , ResponseOk 3 Normalised emptyReport
          ]
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
            liftIO $ Load.loadReportId loaded @?= loadId 1
            failed <- Load.load Load.Request {Load.loadRequestPath = broken}
            liftIO $ case failed of
              Load.ResponseError _ -> pure ()
              other ->
                assertFailure $
                  "expected the second load to fail, got " <> show other
            refused <-
              Goal.goal
                Goal.Request
                  { goalRequestLoadId = loadId 1
                  , goalRequestGoalId = 0
                  , goalRequestNormalization = Simplified
                  }
            liftIO $ refused @?= ResponseRefused (RefusedLoadId NoCurrentLoad)
          _ -> liftIO $ assertFailure "expected two staged fixtures"
    , testCase "a file edited outside the session is refused until reloaded" $
        withFixtureToolSession warm "test/fixtures/HoleNatural.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "the first load"
          liftIO $ Text.IO.appendFile path "\n-- edited in another window\n"
          refused <- Goal.goal (goalRequest $ loadId 1)
          liftIO $ refused @?= ResponseRefused (RefusedSource RefusalChanged)
          reloaded <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "the reload"
          liftIO $ Load.loadReportId reloaded @?= loadId 2
          answered <- Goal.goal (goalRequest $ loadId 2)
          liftIO $ case answered of
            ResponseOk {} -> pure ()
            other ->
              assertFailure $
                "expected the reloaded goal to answer, got " <> show other
    , testCase "a file that has gone missing is refused" $
        withFixtureToolSession warm "test/fixtures/HoleNatural.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          liftIO $ removeFile path
          refused <- Goal.goal (goalRequest $ loadId 1)
          liftIO $ case refused of
            ResponseRefused (RefusedSource (RefusalUnreadable _)) -> pure ()
            other ->
              assertFailure $
                "expected the missing file to be refused, got " <> show other
    , testCase "a goal id that no load reported is unknown" $
        withFixtureToolSession warm "test/fixtures/HoleNatural.agda" $ \path -> do
          loaded <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          liftIO $
            map goalId (Load.loadReportGoals loaded) @?= [0]
          response <-
            Goal.goal
              Goal.Request
                { goalRequestLoadId = loadId 1
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
                { goalRequestLoadId = loadId 1
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

goalRequest :: LoadId -> Request
goalRequest submitted =
  Request
    { goalRequestLoadId = submitted
    , goalRequestGoalId = 0
    , goalRequestNormalization = Simplified
    }

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

plainError :: Text -> Error
plainError message =
  Error
    { errorMessage = message
    , errorPathSpan = Nothing
    , errorWarnings = []
    }

entryValue :: Text -> Text -> Aeson.Value
entryValue name ty =
  Aeson.object
    [ "original_name" Aeson..= name
    , "reified_name" Aeson..= name
    , "original_in_scope" Aeson..= True
    , "reified_in_scope" Aeson..= True
    , "type" Aeson..= ty
    , "let_value" Aeson..= Aeson.Null
    , "is_instance" Aeson..= False
    , "cohesion" Aeson..= Aeson.Null
    , "polarity" Aeson..= Aeson.Null
    , "erased" Aeson..= False
    , "relevance" Aeson..= Aeson.Null
    ]

expectLoadOk :: String -> Load.Response -> IO Load.LoadReport
expectLoadOk _ (Load.ResponseOk report) = pure report
expectLoadOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectGoalOk :: String -> Goal.Response -> IO GoalReport
expectGoalOk _ (ResponseOk _ _ report) = pure report
expectGoalOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other
