{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Check (tests) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.TypeChecking.Monad (TCState)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Corpus qualified as Corpus
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tool.Harness (withFixtureToolSession)

import AgdaMCP.Interaction (
  Error (..),
  GoalReport (..),
  GoalShape (..),
  Warning (..),
 )
import AgdaMCP.Interaction.GoalInfer (Have (..))
import AgdaMCP.Tools.Check (
  CheckReport (..),
  Request (..),
  Response (..),
  renderResponse,
 )
import AgdaMCP.Tools.Check qualified as Check
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadId (..), LoadIdRefusal (..))
import AgdaMCP.Tools.MCP (parseArguments)

tests :: IO TCState -> TestTree
tests warm =
  testGroup
    "Check"
    [ parseArgumentsTests
    , renderResponseTests
    , sessionTests warm
    ]

parseArgumentsTests :: TestTree
parseArgumentsTests =
  testGroup
    "parseArguments"
    [ testCase "a call without an expression is refused" $
        parseRequest
          ( Just $
              Map.fromList
                [ ("load_id", Aeson.String "L1")
                , ("goal", Aeson.Number 0)
                ]
          )
          @?= Left "Error in $: key \"expression\" not found"
    , testCase "a blank expression is refused" $
        parseRequest (arguments "")
          @?= Left
            "Error in $.expression: expected an Agda expression to try at the \
            \goal, but got \"\""
    , testCase "a whitespace-only expression is refused" $
        parseRequest (arguments "  ")
          @?= Left
            "Error in $.expression: expected an Agda expression to try at the \
            \goal, but got \"  \""
    , testCase "normalization defaults to simplified" $
        parseRequest (arguments "suc zero")
          @?= Right (Request (LoadId 1) 0 Simplified "suc zero")
    , testCase "all four arguments parse" $
        parseRequest
          ( Just $
              Map.fromList
                [ ("load_id", Aeson.String "L3")
                , ("goal", Aeson.Number 2)
                , ("normalization", Aeson.String "normalized")
                , ("expression", Aeson.String "suc zero")
                ]
          )
          @?= Right (Request (LoadId 3) 2 Normalised "suc zero")
    ]
 where
  parseRequest :: Maybe (Map.Map Text Aeson.Value) -> Either Text Request
  parseRequest = parseArguments
  arguments expression =
    Just $
      Map.fromList
        [ ("load_id", Aeson.String "L1")
        , ("goal", Aeson.Number 0)
        , ("expression", Aeson.String expression)
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
    , testCase "an unknown goal id points back at the load result" $
        renderResponse (ResponseUnknownGoal 5)
          @?= Left
            "No goal ?5 in the current load. Check the goal IDs in the most \
            \recent result."
    , testCase "a failed query reports Agda's error" $
        renderResponse (ResponseFailed Corpus.typeError)
          @?= Right
            ( rendered
                [ "Check failed:"
                , ""
                , "  /fixture/TypeError.agda:6.9-10: error: [UnequalTerms]"
                , "  Set !=< ℕ"
                , "  when checking that the expression ℕ has type ℕ"
                ]
            )
    , testCase "an expression that fits reports the goal, Have, and elaboration" $
        renderResponse (ResponseOk 0 Simplified fittingReport)
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Have:"
                , "  suc zero"
                , "    : ℕ"
                , ""
                , "Elaborates to:"
                , "  1"
                ]
            )
    , testCase "the header carries the queried goal id and normalization" $
        renderResponse (ResponseOk 3 Normalised fittingReport)
          @?= Right
            ( rendered
                [ "?3 (normalized)"
                , "  ⊢ ℕ"
                , ""
                , "Have:"
                , "  suc zero"
                , "    : ℕ"
                , ""
                , "Elaborates to:"
                , "  1"
                ]
            )
    , testCase "a multi-line expression is echoed verbatim" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              fittingReport
                { checkReportExpression = "λ x\n  → x"
                , checkReportHave = Right (Have "A" [])
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Have:"
                , "  λ x"
                , "    → x"
                , "    : A"
                , ""
                , "Elaborates to:"
                , "  1"
                ]
            )
    , testCase "a multi-line inferred type keeps Agda's own indentation" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              fittingReport {checkReportHave = Right (Have "A\n  → B" [])}
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Have:"
                , "  suc zero"
                , "    : A"
                , "      → B"
                , ""
                , "Elaborates to:"
                , "  1"
                ]
            )
    , testCase "the expression's boundary faces follow in the Have section" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              fittingReport
                { checkReportHave =
                    Right (Have "ℕ" ["i = i0 ⊢ x", "i = i1 ⊢ y"])
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Have:"
                , "  suc zero"
                , "    : ℕ"
                , ""
                , "  i = i0 ⊢ x"
                , ""
                , "  i = i1 ⊢ y"
                , ""
                , "Elaborates to:"
                , "  1"
                ]
            )
    , testCase "an expression that fails to infer reports the error in place" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              fittingReport
                { checkReportHave = Left (plainError "it does not infer")
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Have:"
                , "  it does not infer"
                , ""
                , "Elaborates to:"
                , "  1"
                ]
            )
    , testCase
        "an expression that infers but fails to check reports both as they landed"
        $ renderResponse
          ( ResponseOk
              0
              Simplified
              fittingReport
                { checkReportExpression = "tt"
                , checkReportHave = Right (Have "⊤" [])
                , checkReportChecks = Left (plainError ttCheckError)
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Have:"
                , "  tt"
                , "    : ⊤"
                , ""
                , "Elaborates to:"
                , "  1.1-3: error: [UnequalTerms]"
                , "  ⊤ !=< ℕ"
                , "  when checking that the expression tt has type ℕ"
                ]
            )
    , testCase "a parse error appears in both sections" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              fittingReport
                { checkReportExpression = "suc ("
                , checkReportHave = Left (plainError parseError)
                , checkReportChecks = Left (plainError parseError)
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Have:"
                , "  1.5: error: [ParseError]"
                , "  <EOF><ERROR> ..."
                , ""
                , "Elaborates to:"
                , "  1.5: error: [ParseError]"
                , "  <EOF><ERROR> ..."
                ]
            )
    , testCase "an embedded error's warnings follow as further items" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              fittingReport
                { checkReportHave =
                    Left
                      (plainError "it does not infer")
                        { errorWarnings =
                            [Warning (Nothing, "a warning raised on the way")]
                        }
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Have:"
                , "  it does not infer"
                , ""
                , "  a warning raised on the way"
                , ""
                , "Elaborates to:"
                , "  1"
                ]
            )
    , testCase "goal constraints sit between the goal block and Have" $
        renderResponse
          ( ResponseOk
              0
              Simplified
              fittingReport
                { checkReportGoal =
                    emptyReport {goalReportConstraints = ["a constraint"]}
                }
          )
          @?= Right
            ( rendered
                [ "?0 (simplified)"
                , "  ⊢ ℕ"
                , ""
                , "Constraints:"
                , "  a constraint"
                , ""
                , "Have:"
                , "  suc zero"
                , "    : ℕ"
                , ""
                , "Elaborates to:"
                , "  1"
                ]
            )
    ]

sessionTests :: IO TCState -> TestTree
sessionTests warm =
  testGroup
    "sessions"
    [ testCase "an unknown goal id is answered before the expression is parsed" $
        withFixtureToolSession warm "test/fixtures/InferCheck.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          response <-
            Check.check
              Request
                { checkRequestLoadId = LoadId 1
                , checkRequestGoalId = 5
                , checkRequestNormalization = Simplified
                , checkRequestExpression = "suc ("
                }
          liftIO $ response @?= ResponseUnknownGoal 5
    , testCase "a parse error lands in both fields with expression-relative positions" $
        withFixtureToolSession warm "test/fixtures/InferCheck.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          report <-
            Check.check
              Request
                { checkRequestLoadId = LoadId 1
                , checkRequestGoalId = 0
                , checkRequestNormalization = Simplified
                , checkRequestExpression = "suc ("
                }
              >>= liftIO . expectCheckOk "the unparseable expression"
          liftIO $ do
            goalReportShape (checkReportGoal report) @?= GoalOfType "ℕ"
            checkReportExpression report @?= "suc ("
            first errorMessage (checkReportHave report) @?= Left parseError
            first errorMessage (checkReportChecks report) @?= Left parseError
    , testCase "infer and check disagree by design" $
        withFixtureToolSession warm "test/fixtures/InferCheck.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          report <-
            Check.check
              Request
                { checkRequestLoadId = LoadId 1
                , checkRequestGoalId = 0
                , checkRequestNormalization = Simplified
                , checkRequestExpression = "tt"
                }
              >>= liftIO . expectCheckOk "the expression that infers only"
          liftIO $ do
            goalReportShape (checkReportGoal report) @?= GoalOfType "ℕ"
            checkReportHave report @?= Right (Have "⊤" [])
            first errorMessage (checkReportChecks report)
              @?= Left ttCheckError
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

fittingReport :: CheckReport
fittingReport =
  CheckReport
    { checkReportGoal = emptyReport
    , checkReportExpression = "suc zero"
    , checkReportHave = Right (Have "ℕ" [])
    , checkReportChecks = Right "1"
    }

plainError :: Text -> Error
plainError message =
  Error
    { errorMessage = message
    , errorPathSpan = Nothing
    , errorWarnings = []
    }

-- Observed against InferCheck.agda's ?0 : ℕ at Simplified.

parseError :: Text
parseError = "1.5: error: [ParseError]\n<EOF><ERROR> ..."

ttCheckError :: Text
ttCheckError =
  rendered
    [ "1.1-3: error: [UnequalTerms]"
    , "⊤ !=< ℕ"
    , "when checking that the expression tt has type ℕ"
    ]

expectLoadOk :: String -> Load.Response -> IO Load.LoadReport
expectLoadOk _ (Load.ResponseOk report) = pure report
expectLoadOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectCheckOk :: String -> Check.Response -> IO CheckReport
expectCheckOk _ (ResponseOk _ _ report) = pure report
expectCheckOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other
