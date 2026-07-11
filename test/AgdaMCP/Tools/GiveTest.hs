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
                      "zero"
                  ]
              )
              (Loaded [] [] [] [])
          )
          @?= "Applied 1 give:\n\n\
              \?0 := zero (at 8:12-16)\n\n\
              \File updated and reloaded; interaction IDs may have changed.\n\n\
              \Load succeeded: no goals."
    , testCase "applied batch followed by a reload with a remaining goal" $
        renderGiveResponse
          ( GiveResponse
              ( GiveApplied
                  [ Edit
                      (InteractionId 0)
                      (Span (Position 10 20 4) (Position 15 20 9))
                      "reflexive"
                      "reflexive"
                  , Edit
                      (InteractionId 1)
                      (Span (Position 30 21 4) (Position 35 21 9))
                      "Identity.induction p"
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
          @?= "Applied 2 gives:\n\n\
              \?0 := reflexive (at 20:4-9)\n\
              \?1:\n\
              \  submitted: Identity.induction p\n\
              \  written:   induction p\n\
              \  (at 21:4-9)\n\n\
              \File updated and reloaded; interaction IDs may have changed.\n\n\
              \Load succeeded: 1 goal.\n\n\
              \?0 : P x (at 25:8-13)"
    , testCase "applied give with a multiline elaborated expression" $
        renderGiveResponse
          ( GiveResponse
              ( GiveApplied
                  [ Edit
                      (InteractionId 4)
                      (Span (Position 0 30 8) (Position 5 30 13))
                      "λ x →\n  x"
                      "λ x →\n  x"
                  ]
              )
              (Loaded [] [] [] [])
          )
          @?= "Applied 1 give:\n\n\
              \?4 :=\n\
              \  λ x →\n\
              \    x\n\
              \  (at 30:8-13)\n\n\
              \File updated and reloaded; interaction IDs may have changed.\n\n\
              \Load succeeded: no goals."
    , testCase "applied give followed by a stale reload" $
        renderGiveResponse
          ( GiveResponse
              ( GiveApplied
                  [ Edit
                      (InteractionId 0)
                      (Span (Position 0 8 12) (Position 4 8 16))
                      "zero"
                      "zero"
                  ]
              )
              LoadStale
          )
          @?= "Applied 1 give:\n\n\
              \?0 := zero (at 8:12-16)\n\n\
              \File updated and reloaded; interaction IDs may have changed.\n\n\
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
                      (Just (Span (Position 6 76 27) (Position 11 76 32)))
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
          @?= "Give rejected for ?1 (at 76:27-32).\n\n\
              \Expression error (locations are relative to the submitted expression):\n\n\
              \1.1-2: error: [UnequalTerms]\n\
              \𝟏 !=< true ＝ true\n\
              \when checking that the expression ⋆ has type true ＝ true\n\n\
              \No file changes were made. Reloaded to resync:\n\n\
              \Load succeeded: 2 goals.\n\n\
              \?0 : false ＝ false (at 75:29-34)\n\
              \?1 : true ＝ true (at 76:27-32)"
    , testCase "later give rejected with warnings and earlier gives discarded" $
        renderGiveResponse
          ( GiveResponse
              ( GiveRejected
                  ( RejectedGive
                      (InteractionId 3)
                      (Just (Span (Position 20 40 7) (Position 25 40 12)))
                      ( AgdaError
                          "1.1-5: error: Not in scope: nope"
                          Nothing
                          [ Warning (Nothing, "First warning")
                          , Warning (Nothing, "Second warning")
                          ]
                      )
                      1
                  )
              )
              ( Loaded
                  []
                  []
                  [Warning (Nothing, "Reload warning")]
                  []
              )
          )
          @?= "Give rejected for ?3 (at 40:7-12).\n\n\
              \Expression error (locations are relative to the submitted expression):\n\n\
              \1.1-5: error: Not in scope: nope\n\n\
              \Warnings:\n\n\
              \First warning\n\
              \Second warning\n\n\
              \No file changes were made; 1 earlier give in this call was discarded. Reloaded to resync:\n\n\
              \Load succeeded: no goals, 1 warning.\n\n\
              \Warnings:\n\n\
              \Reload warning"
    , testCase "bogus goal rejected without a target span" $
        renderGiveResponse
          ( GiveResponse
              ( GiveRejected
                  ( RejectedGive
                      (InteractionId 99)
                      Nothing
                      ( AgdaError
                          "No such interaction point: 99"
                          Nothing
                          []
                      )
                      2
                  )
              )
              (Loaded [] [] [] [])
          )
          @?= "Give rejected for ?99.\n\n\
              \Expression error (locations are relative to the submitted expression):\n\n\
              \No such interaction point: 99\n\n\
              \No file changes were made; 2 earlier gives in this call were discarded. Reloaded to resync:\n\n\
              \Load succeeded: no goals."
    , testCase "implicit load error uses its file span" $
        renderGiveResponse
          ( GiveResponse
              ( GiveRejected
                  ( RejectedGive
                      (InteractionId 0)
                      Nothing
                      ( AgdaError
                          "Example.agda:3,1-4\nNot in scope: bad"
                          (Just (Span (Position 10 3 1) (Position 13 3 4)))
                          []
                      )
                      0
                  )
              )
              ( LoadFailed
                  ( AgdaError
                      "Example.agda:3,1-4\nNot in scope: bad"
                      (Just (Span (Position 10 3 1) (Position 13 3 4)))
                      []
                  )
              )
          )
          @?= "Give rejected for ?0.\n\n\
              \Agda error at 3:1-4:\n\n\
              \Example.agda:3,1-4\n\
              \Not in scope: bad\n\n\
              \No file changes were made. Reloaded to resync:\n\n\
              \Load failed:\n\n\
              \Example.agda:3,1-4\n\
              \Not in scope: bad"
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
          @?= "Edit refused for ?0 at 75:29-34.\n\n\
              \The target no longer contains a hole, so the file may have changed since it was loaded. No changes were made.\n\n\
              \Reloaded to resync:\n\n\
              \Load succeeded: 1 goal.\n\n\
              \?0 : false ＝ false (at 76:29-34)"
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
          @?= "Could not access the file on disk:\n\n\
              \Example.agda: permission denied\n\
              \while opening the file\n\n\
              \No changes were written.\n\n\
              \Reloaded to resync:\n\n\
              \Load failed:\n\n\
              \Cannot read file Example.agda"
    ]
