{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.GiveTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Agda.Syntax.Common (InteractionId (InteractionId))

import AgdaMCP.Position (Position (Position), Span (Span))
import AgdaMCP.Tools.Common (
  AgdaError (AgdaError),
  Warning (Warning),
 )
import AgdaMCP.Tools.Give (
  Edit (Edit),
  GiveOutcome (GiveApplied, GiveIOError, GiveRejected, GiveStale),
  GiveResponse (GiveResponse),
  RejectedGive (RejectedGive),
  renderGiveResponse,
 )
import AgdaMCP.Tools.Load (
  Goal (Goal),
  GoalShape (GoalOfType),
  LoadResponse (LoadFailed, LoadStale, Loaded),
 )

tests :: TestTree
tests =
  testGroup
    "renderGiveResponse"
    [ appliedTests
    , rejectedTests
    , staleTests
    , ioErrorTests
    ]

appliedTests :: TestTree
appliedTests =
  testGroup
    "applied gives"
    [ testCase "one applied give followed by a zero-goal reload" $
        renderGiveResponse
          ( GiveResponse
              ( GiveApplied
                  [ Edit
                      (InteractionId 0)
                      (Span (Position 0 8 12) (Position 4 8 16))
                      "zero"
                  ]
              )
              (Loaded [] [] [] [])
          )
          @?= "Gave 1 goal(s):\n\
              \  ?0 := zero\n\
              \The file was updated on disk and reloaded; interaction ids may have changed.\n\n\
              \Load succeeded (no goals)."
    , testCase "applied batch followed by a reload with a remaining goal" $
        renderGiveResponse
          ( GiveResponse
              ( GiveApplied
                  [ Edit
                      (InteractionId 0)
                      (Span (Position 10 20 4) (Position 15 20 9))
                      "reflexive"
                  , Edit
                      (InteractionId 1)
                      (Span (Position 30 21 4) (Position 35 21 9))
                      "induction p"
                  ]
              )
              ( Loaded
                  [ Goal
                      (InteractionId 0)
                      (Span (Position 40 25 8) (Position 45 25 13))
                      (GoalOfType "P x")
                  ]
                  []
                  []
                  []
              )
          )
          @?= "Gave 2 goal(s):\n\
              \  ?0 := reflexive\n\
              \  ?1 := induction p\n\
              \The file was updated on disk and reloaded; interaction ids may have changed.\n\n\
              \?0 : P x (at 25:8-25:13)"
    , testCase "applied give with a multiline elaborated expression" $
        renderGiveResponse
          ( GiveResponse
              ( GiveApplied
                  [ Edit
                      (InteractionId 4)
                      (Span (Position 0 30 8) (Position 5 30 13))
                      "λ x →\n  x"
                  ]
              )
              (Loaded [] [] [] [])
          )
          @?= "Gave 1 goal(s):\n\
              \  ?4 := λ x →\n\
              \  x\n\
              \The file was updated on disk and reloaded; interaction ids may have changed.\n\n\
              \Load succeeded (no goals)."
    , testCase "applied give followed by a stale reload" $
        renderGiveResponse
          ( GiveResponse
              ( GiveApplied
                  [ Edit
                      (InteractionId 0)
                      (Span (Position 0 8 12) (Position 4 8 16))
                      "zero"
                  ]
              )
              LoadStale
          )
          @?= "Gave 1 goal(s):\n\
              \  ?0 := zero\n\
              \The file was updated on disk and reloaded; interaction ids may have changed.\n\n\
              \The file changed on disk while Agda was checking it, so the result was discarded. Please load the file again."
    ]

rejectedTests :: TestTree
rejectedTests =
  testGroup
    "rejected gives"
    [ testCase "first give rejected without warnings" $
        renderGiveResponse
          ( GiveResponse
              ( GiveRejected
                  ( RejectedGive
                      (InteractionId 1)
                      ( AgdaError
                          "1.1-2: error: [UnequalTerms]\n𝟏 !=< true ＝ true\nwhen checking that the expression ⋆ has type true ＝ true"
                          Nothing
                          []
                      )
                      0
                  )
              )
              ( Loaded
                  [ Goal
                      (InteractionId 0)
                      (Span (Position 0 75 29) (Position 5 75 34))
                      (GoalOfType "false ＝ false")
                  , Goal
                      (InteractionId 1)
                      (Span (Position 6 76 27) (Position 11 76 32))
                      (GoalOfType "true ＝ true")
                  ]
                  []
                  []
                  []
              )
          )
          @?= "Give failed for goal ?1:\n\
              \1.1-2: error: [UnequalTerms]\n\
              \𝟏 !=< true ＝ true\n\
              \when checking that the expression ⋆ has type true ＝ true\n\n\
              \The file was left unchanged. Reloaded to resync:\n\n\
              \?0 : false ＝ false (at 75:29-75:34)\n\
              \?1 : true ＝ true (at 76:27-76:32)"
    , testCase "later give rejected with warnings and earlier gives discarded" $
        renderGiveResponse
          ( GiveResponse
              ( GiveRejected
                  ( RejectedGive
                      (InteractionId 3)
                      ( AgdaError
                          "1.1-5: error: Not in scope: nope"
                          Nothing
                          [ Warning (Nothing, "First warning")
                          , Warning (Nothing, "Second warning")
                          ]
                      )
                      2
                  )
              )
              ( Loaded
                  []
                  []
                  [Warning (Nothing, "Reload warning")]
                  []
              )
          )
          @?= "Give failed for goal ?3:\n\
              \1.1-5: error: Not in scope: nope\n\n\
              \Warnings:\n\
              \First warning\n\
              \Second warning\n\n\
              \The file was left unchanged; the 2 earlier give(s) in this call were discarded. Reloaded to resync:\n\n\
              \Load succeeded (no goals).\n\n\
              \Warnings:\n\n\
              \Reload warning"
    ]

staleTests :: TestTree
staleTests =
  testGroup
    "stale edits"
    [ testCase "stale edit refused and reloaded with a shifted goal" $
        renderGiveResponse
          ( GiveResponse
              ( GiveStale
                  ( Edit
                      (InteractionId 0)
                      (Span (Position 100 75 29) (Position 105 75 34))
                      "reflexive"
                  )
              )
              ( Loaded
                  [ Goal
                      (InteractionId 0)
                      (Span (Position 101 76 29) (Position 106 76 34))
                      (GoalOfType "false ＝ false")
                  ]
                  []
                  []
                  []
              )
          )
          @?= "Refused to edit: goal ?0 no longer points at a hole (expected `?` or `{! !}` at 75:29-75:34). The file likely changed on disk since it was loaded. No changes were made; reloaded to resync:\n\n\
              \?0 : false ＝ false (at 76:29-76:34)"
    ]

ioErrorTests :: TestTree
ioErrorTests =
  testGroup
    "file I/O errors"
    [ testCase "file I/O error followed by a failed reload" $
        renderGiveResponse
          ( GiveResponse
              (GiveIOError "Example.agda: permission denied\nwhile opening the file")
              ( LoadFailed
                  (AgdaError "Cannot read file Example.agda" Nothing [])
              )
          )
          @?= "Could not access the file on disk:\n\
              \Example.agda: permission denied\n\
              \while opening the file\n\n\
              \No changes were written. Reloaded to resync:\n\n\
              \Load failed:\n\n\
              \Cannot read file Example.agda"
    ]
