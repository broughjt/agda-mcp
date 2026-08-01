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
    [ testCase "successful load with no goals includes its load id" $
        renderResponse (ResponseOk report)
          @?= Text.unlines
            [ "Load succeeded: no goals."
            , "Load ID: L17"
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
          @?= Text.unlines
            [ "Load succeeded: 1 goal."
            , "Load ID: L17"
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
                          , goalShape =
                              GoalOfType "(y + z) * x ≡ y * x + z * x"
                          }
                      , [contextEntry "x" "ℕ", contextEntry "y" "ℕ", contextEntry "z" "ℕ"]
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
          @?= Text.unlines
            [ "Load succeeded: 2 goals."
            , "Load ID: L17"
            , ""
            , "?0 at 14:16-17"
            , "  x : ℕ"
            , "  y : ℕ"
            , "  z : ℕ"
            , "  ⊢ (y + z) * x ≡ y * x + z * x"
            , ""
            , "?1 at 20:5-6"
            , "  ⊢ ℕ"
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
          @?= Text.unlines
            [ "Load succeeded: 2 goals."
            , "Load ID: L17"
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
          @?= Text.unlines
            [ "Load succeeded: 1 goal."
            , "Load ID: L17"
            , ""
            , "?0 at 14:16-16:4"
            , "  ⊢ ℕ"
            ]
    , testCase "a goal that is itself a sort says so" $
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
          @?= Text.unlines
            [ "Load succeeded: 1 goal."
            , "Load ID: L17"
            , ""
            , "?0 at 3:8-9"
            , "  ⊢ Sort"
            ]
    , testCase "a shadowed binding is reported under both of its names" $
        renderResponse
          ( ResponseOk
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan =
                              Span
                                { spanStart = Position 0 14 16
                                , spanEnd = Position 0 14 17
                                }
                          , goalShape = GoalOfType "ℕ"
                          }
                      ,
                        [ (contextEntry "x" "ℕ")
                            { contextEntryReifiedName = "x₁"
                            , contextEntryReifiedInScope = False
                            }
                        , contextEntry "x" "ℕ"
                        ]
                      )
                    ]
                }
          )
          @?= Text.unlines
            [ "Load succeeded: 1 goal."
            , "Load ID: L17"
            , ""
            , "?0 at 14:16-17"
            , "  x = x₁ : ℕ (not in scope)"
            , "  x : ℕ"
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
                          , goalSpan =
                              Span
                                { spanStart = Position 0 14 16
                                , spanEnd = Position 0 14 17
                                }
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
          @?= Text.unlines
            [ "Load succeeded: 1 goal."
            , "Load ID: L17"
            , ""
            , "?0 at 14:16-17"
            , "  x : ℕ (not in scope)"
            , "  x₁ : ℕ (not in scope)"
            , "  ⊢ ℕ"
            ]
    , testCase "a let binding is reported with its value" $
        renderResponse
          ( ResponseOk $
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan =
                              Span
                                { spanStart = Position 0 14 16
                                , spanEnd = Position 0 14 17
                                }
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
          @?= Text.unlines
            [ "Load succeeded: 1 goal."
            , "Load ID: L17"
            , ""
            , "?0 at 14:16-17"
            , "  m : ℕ"
            , "  doubled : ℕ"
            , "  doubled = m + m"
            , "  ⊢ ℕ"
            ]
    , testCase "binder attributes are reported beside the binding" $
        renderResponse
          ( ResponseOk $
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan =
                              Span
                                { spanStart = Position 0 14 16
                                , spanEnd = Position 0 14 17
                                }
                          , goalShape = GoalOfType "ℕ"
                          }
                      ,
                        [ (contextEntry "eq" "n ≡ n") {contextEntryIsInstance = True}
                        , (contextEntry "n" "ℕ") {contextEntryErased = True}
                        , (contextEntry "i" "ℕ")
                            { contextEntryRelevance = Just "irrelevant"
                            }
                        , (contextEntry "c" "ℕ") {contextEntryCohesion = Just "flat"}
                        , (contextEntry "p" "ℕ") {contextEntryPolarity = Just "unused"}
                        ]
                      )
                    ]
                }
          )
          @?= Text.unlines
            [ "Load succeeded: 1 goal."
            , "Load ID: L17"
            , ""
            , "?0 at 14:16-17"
            , "  eq : n ≡ n (instance)"
            , "  n : ℕ (erased)"
            , "  i : ℕ (irrelevant)"
            , "  c : ℕ (flat)"
            , "  p : ℕ (unused)"
            , "  ⊢ ℕ"
            ]
    , testCase "a binding carrying several attributes lists them all" $
        renderResponse
          ( ResponseOk $
              report
                { loadReportGoals =
                    [
                      ( Goal
                          { goalId = 0
                          , goalSpan =
                              Span
                                { spanStart = Position 0 14 16
                                , spanEnd = Position 0 14 17
                                }
                          , goalShape = GoalOfType "ℕ"
                          }
                      ,
                        [ (contextEntry "eq" "ℕ")
                            { contextEntryType = "n ≡ n"
                            , contextEntryIsInstance = True
                            , contextEntryOriginalInScope = False
                            , contextEntryReifiedInScope = False
                            }
                        ]
                      )
                    ]
                }
          )
          @?= Text.unlines
            [ "Load succeeded: 1 goal."
            , "Load ID: L17"
            , ""
            , "?0 at 14:16-17"
            , "  eq : n ≡ n (instance) (not in scope)"
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
          @?= Text.unlines
            [ "Load succeeded: no goals."
            , "Load ID: L17"
            , ""
            , "Unsolved metavariables:"
            , "  _12 at 9:5-6 : ℕ"
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
          @?= Text.unlines
            [ "Load succeeded: no goals."
            , "Load ID: L17"
            , ""
            , "Warnings:"
            , "  /tmp/Example.agda:8.1-12: warning: -W[no]UnreachableClauses"
            , "  Unreachable clause"
            , ""
            , "  /tmp/Example.agda:13.1-13: warning: -W[no]UnreachableClauses"
            , "  Unreachable clause"
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
          @?= Text.unlines
            [ "Load succeeded: 1 goal."
            , "Load ID: L17"
            , ""
            , "?0 at 14:16-17"
            , "  ⊢ ℕ"
            , ""
            , "Errors:"
            , "  Unsolved constraints"
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
          @?= Text.unlines
            [ "Load succeeded: 1 goal."
            , "Load ID: L17"
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
            , "Errors:"
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
          @?= Text.unlines
            [ "Load failed."
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
                    ]
                }
          )
          @?= Text.unlines
            [ "Load failed."
            , ""
            , "  an error"
            , ""
            , "Warnings:"
            , "  a warning"
            ]
    , testCase "a stale load issues no load id and asks for another load" $
        renderResponse ResponseStale
          @?= Text.unlines
            [ "Load did not finish: the file changed on disk while it was \
              \being checked. Load it again."
            ]
    ]

-- Helpers

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
