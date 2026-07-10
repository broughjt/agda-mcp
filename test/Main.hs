{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Agda.Syntax.Common (InteractionId (InteractionId))

import AgdaMCP.Position (Position (Position), Span (Span))
import AgdaMCP.Tools.Common (
  AgdaError (AgdaError),
  NonFatalError (NonFatalError),
  Warning (Warning),
 )
import AgdaMCP.Tools.Load (
  Goal (Goal),
  GoalShape (GoalOfType, GoalSort),
  HiddenMetavariable (HiddenMetavariable),
  LoadResponse (LoadFailed, LoadStale, Loaded),
  renderLoadResponse,
 )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "renderLoadResponse"
    [ successTests
    , failureTests
    , staleTest
    ]

successTests :: TestTree
successTests =
  testGroup
    "successful loads"
    [ testCase "no open goals" $
        renderLoadResponse (Loaded [] [] [] [])
          @?= "Load succeeded. Open goals: 0."
    , testCase "one typed goal" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 0)
                  (Span (Position 0 8 12) (Position 0 8 16))
                  (GoalOfType "Nat")
              ]
              []
              []
              []
          )
          @?= "Load succeeded. Open goals: 1.\n\
              \?0 : Nat  (at 8:12-8:16)"
    , testCase "multiple goals" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 0)
                  (Span (Position 0 75 29) (Position 0 75 34))
                  (GoalOfType "false ＝ false")
              , Goal
                  (InteractionId 1)
                  (Span (Position 0 76 27) (Position 0 76 32))
                  (GoalOfType "true ＝ true")
              ]
              []
              []
              []
          )
          @?= "Load succeeded. Open goals: 2.\n\
              \?0 : false ＝ false  (at 75:29-75:34)\n\
              \?1 : true ＝ true  (at 76:27-76:32)"
    , testCase "sort goal" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 3)
                  (Span (Position 0 4 7) (Position 0 4 8))
                  GoalSort
              ]
              []
              []
              []
          )
          @?= "Load succeeded. Open goals: 1.\n\
              \Sort ?3  (at 4:7-4:8)"
    , testCase "hidden typed metavariable with a source span" $
        renderLoadResponse
          ( Loaded
              []
              [ HiddenMetavariable
                  "_A_12"
                  (Just (Span (Position 0 3 5) (Position 0 3 6)))
                  (GoalOfType "Set")
              ]
              []
              []
          )
          @?= "Load succeeded. Open goals: 0.\n\n\
              \Unsolved hidden metas:\n\
              \_A_12 : Set  (at 3:5-3:6)"
    , testCase "hidden sort metavariable without a source span" $
        renderLoadResponse
          (Loaded [] [HiddenMetavariable "_a_7" Nothing GoalSort] [] [])
          @?= "Load succeeded. Open goals: 0.\n\n\
              \Unsolved hidden metas:\n\
              \Sort _a_7"
    , testCase "non-fatal errors" $
        renderLoadResponse
          ( Loaded
              []
              []
              []
              [ NonFatalError
                  ( Just (Span (Position 0 10 2) (Position 0 10 9))
                  , "first non-fatal error"
                  )
              , NonFatalError (Nothing, "second non-fatal error")
              ]
          )
          @?= "Load succeeded. Open goals: 0.\n\n\
              \Non-fatal errors:\n\
              \first non-fatal error\n\
              \second non-fatal error"
    , testCase "warnings" $
        renderLoadResponse
          ( Loaded
              []
              []
              [ Warning
                  ( Just (Span (Position 0 12 1) (Position 0 12 6))
                  , "Unreachable clause"
                  )
              , Warning (Nothing, "Import has unsolved metas")
              ]
              []
          )
          @?= "Load succeeded. Open goals: 0.\n\n\
              \Warnings:\n\
              \Unreachable clause\n\
              \Import has unsolved metas"
    , testCase "all success sections in their output order" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 2)
                  (Span (Position 0 20 4) (Position 0 20 9))
                  (GoalOfType "A")
              ]
              [HiddenMetavariable "_B_4" Nothing (GoalOfType "Set₁")]
              [Warning (Nothing, "warning text")]
              [NonFatalError (Nothing, "non-fatal error text")]
          )
          @?= "Load succeeded. Open goals: 1.\n\
              \?2 : A  (at 20:4-20:9)\n\n\
              \Unsolved hidden metas:\n\
              \_B_4 : Set₁\n\n\
              \Non-fatal errors:\n\
              \non-fatal error text\n\n\
              \Warnings:\n\
              \warning text"
    , testCase "multiline rendered payloads" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 4)
                  (Span (Position 0 30 8) (Position 0 30 13))
                  (GoalOfType "A\n  → B")
              ]
              []
              [Warning (Nothing, "warning heading\nwarning detail")]
              []
          )
          @?= "Load succeeded. Open goals: 1.\n\
              \?4 : A\n\
              \  → B  (at 30:8-30:13)\n\n\
              \Warnings:\n\
              \warning heading\n\
              \warning detail"
    ]

failureTests :: TestTree
failureTests =
  testGroup
    "failed loads"
    [ testCase "plain error" $
        renderLoadResponse
          (LoadFailed (AgdaError "Cannot read file Example.agda" Nothing []))
          @?= "Load failed:\nCannot read file Example.agda"
    , testCase "multiline Agda error" $
        renderLoadResponse
          ( LoadFailed
              ( AgdaError
                  "Example.agda:3,1-4\nNot in scope: bad"
                  (Just (Span (Position 0 3 1) (Position 0 3 4)))
                  []
              )
          )
          @?= "Load failed:\n\
              \Example.agda:3,1-4\n\
              \Not in scope: bad"
    , testCase "error with warnings" $
        renderLoadResponse
          ( LoadFailed
              ( AgdaError
                  "Example.agda:7,5-8\nUnequal terms"
                  (Just (Span (Position 0 7 5) (Position 0 7 8)))
                  [ Warning
                      ( Just (Span (Position 0 2 1) (Position 0 2 10))
                      , "Deprecated syntax"
                      )
                  , Warning (Nothing, "Imported module warning")
                  ]
              )
          )
          @?= "Load failed:\n\
              \Example.agda:7,5-8\n\
              \Unequal terms\n\n\
              \Warnings:\n\
              \Deprecated syntax\n\
              \Imported module warning"
    ]

staleTest :: TestTree
staleTest =
  testCase "file changed while Agda was checking" $
    renderLoadResponse LoadStale
      @?= "The file changed on disk while Agda was checking it, so the result \
          \was discarded. Please load the file again."
