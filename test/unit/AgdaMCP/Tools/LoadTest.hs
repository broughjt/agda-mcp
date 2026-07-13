{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.LoadTest (tests) where

import Data.Aeson (toJSON)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Agda.Syntax.Common (InteractionId (InteractionId))

import AgdaMCP.Position (Position (Position), Span (Span))
import AgdaMCP.Tools.Common (
  AgdaError (AgdaError),
  NonFatalError (NonFatalError),
  Warning (Warning),
  parseArguments,
 )
import AgdaMCP.Tools.Load (
  ContextEntry (ContextEntry),
  ContextEntryAttributes (ContextEntryAttributes),
  Goal (Goal),
  GoalShape (GoalOfType, GoalSort),
  HiddenMetavariable (HiddenMetavariable),
  LoadRequest (LoadRequest),
  LoadResponse (LoadFailed, LoadStale, Loaded),
  renderLoadResponse,
 )

tests :: TestTree
tests =
  testGroup
    "load"
    [ renderTests
    , parseArgumentTests
    ]

renderTests :: TestTree
renderTests =
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
          @?= "Load succeeded: no goals."
    , testCase "one typed goal" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 0)
                  (Span (Position 0 8 12) (Position 0 8 16))
                  (GoalOfType "Nat")
                  []
              ]
              []
              []
              []
          )
          @?= "Load succeeded: 1 goal.\n\n\
              \?0 : Nat (at 8:12-16)"
    , testCase "multiple goals" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 0)
                  (Span (Position 0 75 29) (Position 0 75 34))
                  (GoalOfType "false ＝ false")
                  []
              , Goal
                  (InteractionId 1)
                  (Span (Position 0 76 27) (Position 0 76 32))
                  (GoalOfType "true ＝ true")
                  []
              ]
              []
              []
              []
          )
          @?= "Load succeeded: 2 goals.\n\n\
              \?0 : false ＝ false (at 75:29-34)\n\n\
              \?1 : true ＝ true (at 76:27-32)"
    , testCase "sort goal" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 3)
                  (Span (Position 0 4 7) (Position 0 4 8))
                  GoalSort
                  []
              ]
              []
              []
              []
          )
          @?= "Load succeeded: 1 goal.\n\n\
              \Sort ?3 (at 4:7-8)"
    , testCase "goal context renders innermost-first as an indented block" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 0)
                  (Span (Position 0 9 18) (Position 0 9 22))
                  (GoalOfType "Nat")
                  [ ContextEntry "x" True "x" "Nat" Nothing True noAttributes
                  , ContextEntry "y" False "y" "Vec A n" Nothing False noAttributes
                  , ContextEntry
                      "one"
                      True
                      "one"
                      "Nat"
                      (Just "suc zero")
                      True
                      noAttributes
                  ]
              ]
              []
              []
              []
          )
          @?= "Load succeeded: 1 goal.\n\n\
              \?0 : Nat (at 9:18-22)\n\
              \  one : Nat\n\
              \  one = suc zero\n\
              \  y : Vec A n (not in scope)\n\
              \  x : Nat"
    , testCase "shadowed context name displays its reified alias" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 1)
                  (Span (Position 0 4 11) (Position 0 4 15))
                  (GoalOfType "Nat")
                  [ContextEntry "n" True "n₁" "Nat" Nothing True noAttributes]
              ]
              []
              []
              []
          )
          @?= "Load succeeded: 1 goal.\n\n\
              \?1 : Nat (at 4:11-15)\n\
              \  n = n₁ : Nat"
    , testCase "out-of-scope original name displays only the reified name" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 1)
                  (Span (Position 0 4 11) (Position 0 4 15))
                  (GoalOfType "Nat")
                  [ContextEntry "n" False "n₁" "Nat" Nothing True noAttributes]
              ]
              []
              []
              []
          )
          @?= "Load succeeded: 1 goal.\n\n\
              \?1 : Nat (at 4:11-15)\n\
              \  n₁ : Nat"
    , testCase "context binder attributes follow the Emacs layout" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 0)
                  (Span (Position 0 5 3) (Position 0 5 7))
                  (GoalOfType "B")
                  [ ContextEntry
                      "x"
                      True
                      "x₁"
                      "A"
                      Nothing
                      False
                      ( ContextEntryAttributes
                          (Just "@♭")
                          True
                          (Just "irrelevant")
                          (Just "positive")
                          True
                      )
                  ]
              ]
              []
              []
              []
          )
          @?= "Load succeeded: 1 goal.\n\n\
              \?0 : B (at 5:3-7)\n\
              \  @♭ x = x₁ : A (not in scope, erased, irrelevant, positive, instance)"
    , testCase "multiline context type keeps the block indentation" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 0)
                  (Span (Position 0 6 3) (Position 0 6 7))
                  (GoalOfType "B")
                  [ContextEntry "f" True "f" "A\n→ B" Nothing True noAttributes]
              ]
              []
              []
              []
          )
          @?= "Load succeeded: 1 goal.\n\n\
              \?0 : B (at 6:3-7)\n\
              \  f : A\n\
              \  → B"
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
          @?= "Load succeeded: no goals, 1 unsolved metavariable.\n\n\
              \Unsolved metavariables:\n\n\
              \_A_12 : Set (at 3:5-6)"
    , testCase "hidden sort metavariable without a source span" $
        renderLoadResponse
          (Loaded [] [HiddenMetavariable "_a_7" Nothing GoalSort] [] [])
          @?= "Load succeeded: no goals, 1 unsolved metavariable.\n\n\
              \Unsolved metavariables:\n\n\
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
          @?= "Load completed with 2 non-fatal errors: no goals.\n\n\
              \Non-fatal errors:\n\n\
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
          @?= "Load succeeded: no goals, 2 warnings.\n\n\
              \Warnings:\n\n\
              \Unreachable clause\n\
              \Import has unsolved metas"
    , testCase "all success sections in their output order" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 2)
                  (Span (Position 0 20 4) (Position 0 20 9))
                  (GoalOfType "A")
                  []
              ]
              [HiddenMetavariable "_B_4" Nothing (GoalOfType "Set₁")]
              [Warning (Nothing, "warning text")]
              [NonFatalError (Nothing, "non-fatal error text")]
          )
          @?= "Load completed with 1 non-fatal error: \
              \1 goal, 1 unsolved metavariable, 1 warning.\n\n\
              \?2 : A (at 20:4-9)\n\n\
              \Unsolved metavariables:\n\n\
              \_B_4 : Set₁\n\n\
              \Non-fatal errors:\n\n\
              \non-fatal error text\n\n\
              \Warnings:\n\n\
              \warning text"
    , testCase "multiline rendered payloads" $
        renderLoadResponse
          ( Loaded
              [ Goal
                  (InteractionId 4)
                  (Span (Position 0 30 8) (Position 0 30 13))
                  (GoalOfType "A\n  → B")
                  []
              ]
              []
              [Warning (Nothing, "warning heading\nwarning detail")]
              []
          )
          @?= "Load succeeded: 1 goal, 1 warning.\n\n\
              \?4 : A\n\
              \  → B (at 30:8-13)\n\n\
              \Warnings:\n\n\
              \warning heading\n\
              \warning detail"
    ]

noAttributes :: ContextEntryAttributes
noAttributes = ContextEntryAttributes Nothing False Nothing Nothing False

failureTests :: TestTree
failureTests =
  testGroup
    "failed loads"
    [ testCase "plain error" $
        renderLoadResponse
          (LoadFailed (AgdaError "Cannot read file Example.agda" Nothing []))
          @?= "Load failed:\n\nCannot read file Example.agda"
    , testCase "multiline Agda error" $
        renderLoadResponse
          ( LoadFailed
              ( AgdaError
                  "Example.agda:3,1-4\nNot in scope: bad"
                  (Just (Span (Position 0 3 1) (Position 0 3 4)))
                  []
              )
          )
          @?= "Load failed:\n\n\
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
          @?= "Load failed:\n\n\
              \Example.agda:7,5-8\n\
              \Unequal terms\n\n\
              \Warnings:\n\n\
              \Deprecated syntax\n\
              \Imported module warning"
    ]

staleTest :: TestTree
staleTest =
  testCase "file changed while Agda was checking" $
    renderLoadResponse LoadStale
      @?= "The file changed on disk while Agda was checking it, so the result \
          \was discarded. Please load the file again."

parseArgumentTests :: TestTree
parseArgumentTests =
  testGroup
    "parseArguments"
    [ testCase "valid request" $
        case parseArguments (Just (Map.fromList [("path", "/tmp/Hole.agda")])) of
          Right (LoadRequest path) -> path @?= "/tmp/Hole.agda"
          Left message ->
            assertFailure ("unexpected parse failure: " <> Text.unpack message)
    , testCase "non-string path" $
        case parseArguments (Just (Map.fromList [("path", toJSON (42 :: Int))])) of
          Right (LoadRequest _) ->
            assertFailure "expected a parse failure, got a parsed request"
          Left message ->
            assertBool
              ("expected the failure message to mention $.path, got: " <> Text.unpack message)
              ("$.path" `Text.isInfixOf` (message :: Text))
    ]
