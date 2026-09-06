{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Load (tests) where

import Agda.Interaction.Options (defaultOptions)
import Control.Concurrent.MVar (newMVar)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AgdaMCP.Interaction (
  Error (..),
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  NonFatalError (..),
  Position (..),
  Span (..),
  Warning (..),
 )
import AgdaMCP.Tools.Load (
  LoadReport (..),
  Request (..),
  Response (..),
  loadTool,
  renderResponse,
 )
import AgdaMCP.Tools.State (newToolState)
import AgdaMCP.Tools.Tool (Answer (..), Tool (..), parseArguments)
import Test.Corpus qualified as Corpus
import Test.Tool.Harness (describedKinds, loadId)

tests :: TestTree
tests =
  testGroup
    "Load"
    [ parseArgumentsTests
    , toolCallTests
    , renderResponseTests
    , toJSONTests
    ]

parseArgumentsTests :: TestTree
parseArgumentsTests =
  testGroup
    "parseArguments"
    [ testCase "a call without a path is refused" $
        parseRequest Map.empty
          @?= Left "Error in $: key \"path\" not found"
    , testCase "a path that is not a string is refused" $
        parseRequest (Map.fromList [("path", Aeson.Number 5)])
          @?= Left "Error in $.path: expected String, but encountered Number"
    , testCase "a string path parses" $
        parseRequest
          (Map.fromList [("path", Aeson.String "/tmp/Example.agda")])
          @?= Right (Request "/tmp/Example.agda")
    ]
 where
  parseRequest :: Map.Map Text Aeson.Value -> Either Text Request
  parseRequest = parseArguments

toolCallTests :: TestTree
toolCallTests =
  testGroup
    "toolCall"
    [ testCase "arguments that fail to decode answer invalid_arguments" $ do
        stateVariable <- newToolState defaultOptions >>= newMVar
        answer <- toolCall loadTool stateVariable Map.empty
        answer @?= invalidArguments "Error in $: key \"path\" not found"
    , testCase "a timeout that fails to parse answers invalid_arguments" $ do
        stateVariable <- newToolState defaultOptions >>= newMVar
        answer <-
          toolCall
            loadTool
            stateVariable
            ( Map.fromList
                [ ("path", Aeson.String "/tmp/Example.agda")
                , ("timeout", Aeson.Number 0)
                ]
            )
        answer
          @?= invalidArguments
            "expected timeout to be a whole number of seconds greater than zero"
    ]
 where
  invalidArguments :: Text -> Answer
  invalidArguments message =
    Answer
      { answerOk = False
      , answerText = message
      , answerValue =
          Aeson.object
            [ "kind" Aeson..= Aeson.String "invalid_arguments"
            , "message" Aeson..= message
            ]
      }

renderResponseTests :: TestTree
renderResponseTests =
  testGroup
    "renderResponse"
    [ testCase "a successful load reports its load id and the file Agda loaded" $
        renderResponse (ResponseOk report)
          @?= rendered
            [ "Load succeeded: no goals"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            ]
    , testCase "a single open goal is counted in the singular" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [ Goal
                        { goalId = 0
                        , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                        , goalShape = GoalOfType "ℕ"
                        }
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17 : ℕ"
            ]
    , testCase "each goal is one line and they form one block" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [ Goal
                        { goalId = 0
                        , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                        , goalShape = GoalOfType "(y + z) * x ≡ y * x + z * x"
                        }
                    , Goal
                        { goalId = 1
                        , goalSpan = Span (Position 0 20 5) (Position 0 20 6)
                        , goalShape = GoalOfType "ℕ"
                        }
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 2 goals"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17 : (y + z) * x ≡ y * x + z * x"
            , "?1 at 20:5-6 : ℕ"
            ]
    , testCase "goals keep their own ids in the order they are given" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [ Goal
                        { goalId = 1
                        , goalSpan = Span (Position 0 5 10) (Position 0 5 11)
                        , goalShape = GoalOfType "ℕ"
                        }
                    , Goal
                        { goalId = 0
                        , goalSpan = Span (Position 0 9 3) (Position 0 9 4)
                        , goalShape = GoalOfType "ℕ"
                        }
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 2 goals"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "?1 at 5:10-11 : ℕ"
            , "?0 at 9:3-4 : ℕ"
            ]
    , testCase "a goal that spans lines reports both of them" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [ Goal
                        { goalId = 0
                        , goalSpan = Span (Position 0 14 16) (Position 0 16 4)
                        , goalShape = GoalOfType "ℕ"
                        }
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-16:4 : ℕ"
            ]
    , testCase "a goal that is a sort says so" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [ Goal
                        { goalId = 0
                        , goalSpan = Span (Position 0 3 8) (Position 0 3 9)
                        , goalShape = GoalSort
                        }
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 3:8-9 : Sort"
            ]
    , testCase "a goal type that spans lines keeps Agda's own indentation" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [ Goal
                        { goalId = 0
                        , goalSpan = Span (Position 0 30 8) (Position 0 30 13)
                        , goalShape = GoalOfType "A\n  → B"
                        }
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 30:8-13 : A"
            , "                  → B"
            ]
    , testCase "a goal type that spans lines continues under its start" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [ Goal
                        { goalId = 0
                        , goalSpan = Span (Position 0 30 8) (Position 0 30 13)
                        , goalShape = GoalOfType "A\n→ B"
                        }
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 30:8-13 : A"
            , "                → B"
            ]
    , testCase "a hidden metavariable type that spans lines continues under its start" $
        renderResponse
          ( ResponseOk
              report
                { loadReportHiddenMetavariables =
                    [ HiddenMetavariable
                        { hiddenMetavariableName = "_12"
                        , hiddenMetavariableSpan =
                            Just (Span (Position 0 9 5) (Position 0 9 6))
                        , hiddenMetavariableShape = GoalOfType "A\n→ B"
                        }
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: no goals"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "Unsolved metavariables:"
            , "  _12 at 9:5-6 : A"
            , "                 → B"
            ]
    , testCase "unsolved hidden metavariables are reported in their own section" $
        renderResponse
          ( ResponseOk
              report
                { loadReportHiddenMetavariables =
                    [ HiddenMetavariable
                        { hiddenMetavariableName = "_12"
                        , hiddenMetavariableSpan =
                            Just (Span (Position 0 9 5) (Position 0 9 6))
                        , hiddenMetavariableShape = GoalOfType "ℕ"
                        }
                    , HiddenMetavariable
                        { hiddenMetavariableName = "_14"
                        , hiddenMetavariableSpan = Nothing
                        , hiddenMetavariableShape = GoalSort
                        }
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: no goals"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "Unsolved metavariables:"
            , "  _12 at 9:5-6 : ℕ"
            , ""
            , "  _14 : Sort"
            ]
    , testCase "warnings are reported as Agda wrote them" $
        renderResponse
          ( ResponseOk
              report
                { loadReportPath = Corpus.fixtureFile "Warnings.agda"
                , loadReportWarnings = Corpus.unreachableClauseWarnings
                }
          )
          @?= rendered
            [ "Load succeeded: no goals"
            , "Load ID: L17-k7x2"
            , "File: /fixture/Warnings.agda"
            , ""
            , "Warnings:"
            , "  /fixture/Warnings.agda:8.1-12: warning: -W[no]UnreachableClauses"
            , "  Unreachable clause"
            , "  when checking the definition of first"
            , ""
            , "  /fixture/Warnings.agda:13.1-13: warning: -W[no]UnreachableClauses"
            , "  Unreachable clause"
            , "  when checking the definition of second"
            ]
    , testCase "each warning is separated from the next" $
        renderResponse
          ( ResponseOk
              report
                { loadReportWarnings =
                    [ Warning
                        ( Just
                            ( "/tmp/Example.agda"
                            , Span (Position 0 8 1) (Position 0 8 12)
                            )
                        , "a warning"
                        )
                    , Warning (Nothing, "another warning")
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: no goals"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "Warnings:"
            , "  a warning"
            , ""
            , "  another warning"
            ]
    , testCase "a non-fatal error leaves the load successful and its goals open" $
        renderResponse
          ( ResponseOk
              report
                { loadReportPath = Corpus.fixtureFile "Constrained.agda"
                , loadReportGoals = [Corpus.constrainedGoal]
                , loadReportNonFatalErrors = [Corpus.unsolvedConstraints]
                }
          )
          @?= rendered
            [ "Load succeeded with errors: 1 goal"
            , "Load ID: L17-k7x2"
            , "File: /fixture/Constrained.agda"
            , ""
            , "?0 at 12:17-18 : ℕ"
            , ""
            , "Non-fatal errors:"
            , "  error: [UnsolvedConstraints]"
            , "  Failed to solve the following constraints:"
            , "    ?0 + ?0 = 4 : ℕ (blocked on _n_4)"
            ]
    , testCase "a non-fatal error keeps Agda's own indentation" $
        renderResponse
          ( ResponseOk
              report
                { loadReportPath = Corpus.fixtureFile "SafePostulate.agda"
                , loadReportNonFatalErrors = [Corpus.safeFlagPostulate]
                }
          )
          @?= rendered
            [ "Load succeeded with errors: no goals"
            , "Load ID: L17-k7x2"
            , "File: /fixture/SafePostulate.agda"
            , ""
            , "Non-fatal errors:"
            , "  /fixture/SafePostulate.agda:4.3-24: error: [SafeFlagPostulate]"
            , "  Cannot postulate cheat with safe flag"
            , "  when scope checking the declaration"
            , "    cheat : {A : Set} → A"
            ]
    , testCase "several non-fatal errors are reported in the order they were raised" $
        renderResponse
          ( ResponseOk
              report
                { loadReportNonFatalErrors =
                    [ NonFatalError (Nothing, "first non-fatal error")
                    , NonFatalError
                        ( Just
                            ( "/tmp/Example.agda"
                            , Span (Position 0 10 2) (Position 0 10 9)
                            )
                        , "second non-fatal error"
                        )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded with errors: no goals"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "Non-fatal errors:"
            , "  first non-fatal error"
            , ""
            , "  second non-fatal error"
            ]
    , testCase "the sections are reported in a fixed order" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [ Goal
                        { goalId = 0
                        , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                        , goalShape = GoalOfType "ℕ"
                        }
                    ]
                , loadReportHiddenMetavariables =
                    [ HiddenMetavariable
                        { hiddenMetavariableName = "_12"
                        , hiddenMetavariableSpan = Nothing
                        , hiddenMetavariableShape = GoalOfType "ℕ"
                        }
                    ]
                , loadReportWarnings =
                    [ Warning
                        ( Just
                            ( "/tmp/Example.agda"
                            , Span (Position 0 8 1) (Position 0 8 12)
                            )
                        , "a warning"
                        )
                    ]
                , loadReportNonFatalErrors =
                    [NonFatalError (Nothing, "Unsolved constraints")]
                }
          )
          @?= rendered
            [ "Load succeeded with errors: 1 goal"
            , "Load ID: L17-k7x2"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17 : ℕ"
            , ""
            , "Unsolved metavariables:"
            , "  _12 : ℕ"
            , ""
            , "Warnings:"
            , "  a warning"
            , ""
            , "Non-fatal errors:"
            , "  Unsolved constraints"
            ]
    , testCase "a failed load reports the error and issues no load id" $
        renderResponse (ResponseError Corpus.typeError)
          @?= rendered
            [ "Load failed:"
            , ""
            , "  /fixture/TypeError.agda:6.9-10: error: [UnequalTerms]"
            , "  Set !=< ℕ"
            , "  when checking that the expression ℕ has type ℕ"
            ]
    , testCase "a failed load reports the warnings raised before the error" $
        renderResponse (ResponseError Corpus.warningThenError)
          @?= rendered
            [ "Load failed:"
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
    , testCase "a stale load issues no load id and asks for another load" $
        renderResponse ResponseStale
          @?= "Load did not finish: the file changed on disk while it was \
              \being checked. Load it again."
    ]

toJSONTests :: TestTree
toJSONTests =
  testGroup
    "toJSON"
    [ testCase "a load with nothing to report has every section, empty" $
        Aeson.toJSON (ResponseOk report)
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "loaded"
            , "load_id" Aeson..= Aeson.String "L17-k7x2"
            , "path" Aeson..= Aeson.String "/tmp/Example.agda"
            , "goals" Aeson..= ([] :: [Aeson.Value])
            , "hidden_metavariables" Aeson..= ([] :: [Aeson.Value])
            , "warnings" Aeson..= ([] :: [Aeson.Value])
            , "non_fatal_errors" Aeson..= ([] :: [Aeson.Value])
            ]
    , testCase "every section is encoded" $
        Aeson.toJSON
          ( ResponseOk
              report
                { loadReportGoals =
                    [ Goal
                        { goalId = 0
                        , goalSpan = Span (Position 0 14 16) (Position 1 14 17)
                        , goalShape = GoalOfType "ℕ"
                        }
                    ]
                , loadReportHiddenMetavariables =
                    [ HiddenMetavariable
                        { hiddenMetavariableName = "_12"
                        , hiddenMetavariableSpan = Nothing
                        , hiddenMetavariableShape = GoalSort
                        }
                    ]
                , loadReportWarnings =
                    [ Warning
                        ( Just
                            ( "/tmp/Example.agda"
                            , Span (Position 20 8 1) (Position 31 8 12)
                            )
                        , "a warning"
                        )
                    ]
                , loadReportNonFatalErrors =
                    [NonFatalError (Nothing, "Unsolved constraints")]
                }
          )
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "loaded"
            , "load_id" Aeson..= Aeson.String "L17-k7x2"
            , "path" Aeson..= Aeson.String "/tmp/Example.agda"
            , "goals"
                Aeson..= [ Aeson.object
                             [ "id" Aeson..= (0 :: Int)
                             , "span" Aeson..= spanValue (0, 14, 16) (1, 14, 17)
                             , "shape"
                                 Aeson..= Aeson.object
                                   [ "kind" Aeson..= Aeson.String "of_type"
                                   , "type" Aeson..= Aeson.String "ℕ"
                                   ]
                             ]
                         ]
            , "hidden_metavariables"
                Aeson..= [ Aeson.object
                             [ "name" Aeson..= Aeson.String "_12"
                             , "span" Aeson..= Aeson.Null
                             , "shape"
                                 Aeson..= Aeson.object
                                   ["kind" Aeson..= Aeson.String "sort"]
                             ]
                         ]
            , "warnings"
                Aeson..= [ Aeson.object
                             [ "message" Aeson..= Aeson.String "a warning"
                             , "location"
                                 Aeson..= Aeson.object
                                   [ "path" Aeson..= Aeson.String "/tmp/Example.agda"
                                   , "span" Aeson..= spanValue (20, 8, 1) (31, 8, 12)
                                   ]
                             ]
                         ]
            , "non_fatal_errors"
                Aeson..= [ Aeson.object
                             [ "message" Aeson..= Aeson.String "Unsolved constraints"
                             , "location" Aeson..= Aeson.Null
                             ]
                         ]
            ]
    , testCase "a failed load carries the error and its location" $
        Aeson.toJSON (ResponseError Corpus.typeError)
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "failed"
            , "error"
                Aeson..= Aeson.object
                  [ "message" Aeson..= errorMessage Corpus.typeError
                  , "location"
                      Aeson..= Aeson.object
                        [ "path" Aeson..= Corpus.fixtureFile "TypeError.agda"
                        , "span" Aeson..= spanValue (76, 6, 9) (77, 6, 10)
                        ]
                  , "warnings" Aeson..= ([] :: [Aeson.Value])
                  ]
            ]
    , testCase "a stale load is file_changed" $
        Aeson.toJSON ResponseStale
          @?= Aeson.object ["kind" Aeson..= Aeson.String "file_changed"]
    , testCase "every kind is described" $
        describedKinds
          loadTool
          (Right . renderResponse)
          [ResponseOk report, ResponseError Corpus.typeError, ResponseStale]
    ]

-- Helpers

spanValue :: (Int, Int, Int) -> (Int, Int, Int) -> Aeson.Value
spanValue start end =
  Aeson.object ["start" Aeson..= position start, "end" Aeson..= position end]
 where
  position (offset, line, column) =
    Aeson.object
      [ "offset" Aeson..= offset
      , "line" Aeson..= line
      , "column" Aeson..= column
      ]

rendered :: [Text] -> Text
rendered = Text.intercalate "\n"

report :: LoadReport
report =
  LoadReport
    { loadReportId = loadId 17
    , loadReportPath = "/tmp/Example.agda"
    , loadReportGoals = []
    , loadReportHiddenMetavariables = []
    , loadReportWarnings = []
    , loadReportNonFatalErrors = []
    }
