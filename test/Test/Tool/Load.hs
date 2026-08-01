{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Load (tests) where

import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AgdaMCP.Interaction.Model (
  ContextEntry (..),
  Error (..),
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  NonFatalError (..),
  Position (..),
  Span (..),
  Warning (..),
 )
import AgdaMCP.Tools.Internal (LoadId (..))
import AgdaMCP.Tools.Load (LoadReport (..), Response (..), renderResponse)

tests :: TestTree
tests =
  testGroup
    "Load"
    [ renderResponseTests
    ]

renderResponseTests :: TestTree
renderResponseTests =
  testGroup
    "renderResponse"
    [ testCase "a successful load reports its load id and the file Agda loaded" $
        renderResponse (ResponseOk report)
          @?= rendered
            [ "Load succeeded: no goals"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            ]
    , testCase "a single open goal is counted in the singular" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
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
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  ⊢ ℕ"
            ]
    , testCase "each goal is reported with its context above its type" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      , [contextEntry "x" "ℕ"]
                      )
                    ,
                      ( Goal
                          { goalId = 1
                          , goalSpan = Span (Position 0 20 5) (Position 0 20 6)
                          , goalShape = GoalOfType "ℕ"
                          }
                      , []
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 2 goals"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  x : ℕ"
            , "  ⊢ ℕ"
            , ""
            , "?1 at 20:5-6"
            , "  ⊢ ℕ"
            ]
    , testCase "context entries are reported innermost-first" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape =
                              GoalOfType "(y + z) * x ≡ y * x + z * x"
                          }
                      , [contextEntry "x" "ℕ", contextEntry "y" "ℕ", contextEntry "z" "ℕ"]
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  z : ℕ"
            , "  y : ℕ"
            , "  x : ℕ"
            , "  ⊢ (y + z) * x ≡ y * x + z * x"
            ]
    , testCase "goals keep their own ids in the order they are given" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 1
                          , goalSpan = Span (Position 0 5 10) (Position 0 5 11)
                          , goalShape = GoalOfType "ℕ"
                          }
                      , []
                      )
                    ,
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 9 3) (Position 0 9 4)
                          , goalShape = GoalOfType "ℕ"
                          }
                      , []
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 2 goals"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?1 at 5:10-11"
            , "  ⊢ ℕ"
            , ""
            , "?0 at 9:3-4"
            , "  ⊢ ℕ"
            ]
    , testCase "a goal that spans lines reports both of them" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 16 4)
                          , goalShape = GoalOfType "ℕ"
                          }
                      , []
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-16:4"
            , "  ⊢ ℕ"
            ]
    , testCase "a goal that is a sort says so" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 3 8) (Position 0 3 9)
                          , goalShape = GoalSort
                          }
                      , []
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 3:8-9"
            , "  ⊢ Sort"
            ]
    , testCase "a goal type that spans lines keeps Agda's own indentation" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 30 8) (Position 0 30 13)
                          , goalShape = GoalOfType "A\n  → B"
                          }
                      , []
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 30:8-13"
            , "  ⊢ A"
            , "    → B"
            ]
    , testCase "a shadowed binding is reported under both of its names" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      , [(contextEntry "x" "ℕ") {contextEntryReifiedName = "x₁"}]
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  x = x₁ : ℕ"
            , "  ⊢ ℕ"
            ]
    , testCase "a shadowed binding that has left scope says so" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      ,
                        [ (contextEntry "x" "ℕ")
                            { contextEntryReifiedName = "x₁"
                            , contextEntryReifiedInScope = False
                            }
                        ]
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  x = x₁ : ℕ (not in scope)"
            , "  ⊢ ℕ"
            ]
    , testCase "a binder out of scope is reported under its reified name" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      ,
                        [ (contextEntry "x" "ℕ")
                            { contextEntryReifiedName = "x₁"
                            , contextEntryOriginalInScope = False
                            }
                        ]
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  x₁ : ℕ"
            , "  ⊢ ℕ"
            ]
    , testCase "two anonymous binders are told apart by their reified names" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      ,
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
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  x₁ : ℕ (not in scope)"
            , "  x : ℕ (not in scope)"
            , "  ⊢ ℕ"
            ]
    , testCase "a let binding is reported with its value" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      ,
                        [ contextEntry "m" "ℕ"
                        , (contextEntry "doubled" "ℕ")
                            { contextEntryLetValue = Just "m + m"
                            }
                        ]
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  doubled : ℕ"
            , "  doubled = m + m"
            , "  m : ℕ"
            , "  ⊢ ℕ"
            ]
    , testCase "binder attributes are reported beside the binding" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      ,
                        [ (contextEntry "eq" "n ≡ n") {contextEntryIsInstance = True}
                        , (contextEntry "n" "ℕ") {contextEntryErased = True}
                        , (contextEntry "i" "ℕ") {contextEntryRelevance = Just "irrelevant"}
                        , (contextEntry "c" "ℕ") {contextEntryCohesion = Just "@♭"}
                        , (contextEntry "p" "ℕ") {contextEntryPolarity = Just "unused"}
                        ]
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  p : ℕ (unused)"
            , "  @♭ c : ℕ"
            , "  i : ℕ (irrelevant)"
            , "  n : ℕ (erased)"
            , "  eq : n ≡ n (instance)"
            , "  ⊢ ℕ"
            ]
    , testCase "a binding carrying several attributes lists them in one group" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      ,
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
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  @♭ x = x₁ : A (not in scope, erased, irrelevant, positive, instance)"
            , "  ⊢ ℕ"
            ]
    , testCase "a context type that spans lines keeps Agda's own indentation" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      ,
                        [ (contextEntry "f" "A\n  → B") {contextEntryIsInstance = True}
                        ]
                      )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  f : A"
            , "    → B (instance)"
            , "  ⊢ ℕ"
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
            , "Load ID: L17"
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
                { loadReportWarnings =
                    [ Warning
                        ( Just
                            ( "/tmp/Example.agda"
                            , Span (Position 0 8 1) (Position 0 8 12)
                            )
                        , "/tmp/Example.agda:8.1-12: warning: -W[no]UnreachableClauses\n\
                          \Unreachable clause"
                        )
                    , Warning
                        ( Just
                            ( "/tmp/Example.agda"
                            , Span (Position 0 13 1) (Position 0 13 13)
                            )
                        , "/tmp/Example.agda:13.1-13: warning: -W[no]UnreachableClauses\n\
                          \Unreachable clause"
                        )
                    ]
                }
          )
          @?= rendered
            [ "Load succeeded: no goals"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "Warnings:"
            , "  /tmp/Example.agda:8.1-12: warning: -W[no]UnreachableClauses"
            , "  Unreachable clause"
            , ""
            , "  /tmp/Example.agda:13.1-13: warning: -W[no]UnreachableClauses"
            , "  Unreachable clause"
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
            , "Load ID: L17"
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
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      , []
                      )
                    ]
                , loadReportNonFatalErrors =
                    [NonFatalError (Nothing, "Unsolved constraints")]
                }
          )
          @?= rendered
            [ "Load succeeded with errors: 1 goal"
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  ⊢ ℕ"
            , ""
            , "Non-fatal errors:"
            , "  Unsolved constraints"
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
            , "Load ID: L17"
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
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan = Span (Position 0 14 16) (Position 0 14 17)
                          , goalShape = GoalOfType "ℕ"
                          }
                      , [contextEntry "x" "ℕ"]
                      )
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
            , "Load ID: L17"
            , "File: /tmp/Example.agda"
            , ""
            , "?0 at 14:16-17"
            , "  x : ℕ"
            , "  ⊢ ℕ"
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
        renderResponse
          ( ResponseError
              Error
                { errorMessage =
                    "/tmp/Example.agda:6.9-10: error: [UnequalTerms]\n\
                    \ℕ != Set₁"
                , errorPathSpan =
                    Just
                      ( "/tmp/Example.agda"
                      , Span (Position 0 6 9) (Position 0 6 10)
                      )
                , errorWarnings = []
                }
          )
          @?= rendered
            [ "Load failed:"
            , ""
            , "  /tmp/Example.agda:6.9-10: error: [UnequalTerms]"
            , "  ℕ != Set₁"
            ]
    , testCase "a failed load reports the warnings raised before the error" $
        renderResponse
          ( ResponseError
              Error
                { errorMessage = "an error"
                , errorPathSpan = Nothing
                , errorWarnings =
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
            [ "Load failed:"
            , ""
            , "  an error"
            , ""
            , "Warnings:"
            , "  a warning"
            , ""
            , "  another warning"
            ]
    , testCase "a stale load issues no load id and asks for another load" $
        renderResponse ResponseStale
          @?= "Load did not finish: the file changed on disk while it was \
              \being checked. Load it again."
    ]

-- Helpers

rendered :: [Text] -> Text
rendered = Text.intercalate "\n"

report :: LoadReport
report =
  LoadReport
    { loadReportId = LoadId 17
    , loadReportPath = "/tmp/Example.agda"
    , loadReportGoals = []
    , loadReportHiddenMetavariables = []
    , loadReportWarnings = []
    , loadReportNonFatalErrors = []
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
