{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.GiveTest (tests) where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Agda.Syntax.Common (InteractionId (InteractionId))

import AgdaMCP.Position (Position (Position), Span (Span))
import AgdaMCP.Tools.Common (
  AgdaError (AgdaError),
  Warning (Warning),
  parseArguments,
 )
import AgdaMCP.Tools.Give (
  BatchPosition (BatchPosition),
  Edit (Edit),
  GiveOutcome (
    GiveApplied,
    GiveFileChanged,
    GiveIOError,
    GiveNotLoaded,
    GiveRejected,
    GiveUnknownGoal
  ),
  GiveRejection (GiveRejection),
  GiveRequest (GiveRequest),
  GiveResponse (GiveResponse),
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
    "give"
    [ renderTests
    , parseArgumentTests
    ]

renderTests :: TestTree
renderTests =
  testGroup
    "renderGiveResponse"
    [ appliedTests
    , rejectedTests
    , unknownGoalTests
    , notLoadedTests
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
              \?0 := zero (was at 8:12-16)\n\n\
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
                      []
                  ]
                  []
                  []
                  []
              )
          )
          @?= "Applied 2 gives:\n\n\
              \?0 := reflexive (was at 20:4-9)\n\
              \?1:\n\
              \  submitted: Identity.induction p\n\
              \  written:   induction p\n\
              \  (was at 21:4-9)\n\n\
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
              \  (was at 30:8-13)\n\n\
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
              \?0 := zero (was at 8:12-16)\n\n\
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
                  ( GiveRejection
                      (InteractionId 1)
                      (Just (Span (Position 6 76 27) (Position 11 76 32)))
                      ( AgdaError
                          "1.1-2: error: [UnequalTerms]\n𝟏 !=< true ＝ true\nwhen checking that the expression ⋆ has type true ＝ true"
                          Nothing
                          []
                      )
                      (BatchPosition 0 1)
                  )
              )
              ( Loaded
                  [ Goal
                      (InteractionId 0)
                      (Span (Position 0 75 29) (Position 5 75 34))
                      (GoalOfType "false ＝ false")
                      []
                  , Goal
                      (InteractionId 1)
                      (Span (Position 6 76 27) (Position 11 76 32))
                      (GoalOfType "true ＝ true")
                      []
                  ]
                  []
                  []
                  []
              )
          )
          @?= "Give rejected for ?1 (at 76:27-32; give 1 of 1). No file changes were made.\n\n\
              \Expression error (locations are relative to the submitted expression):\n\n\
              \1.1-2: error: [UnequalTerms]\n\
              \𝟏 !=< true ＝ true\n\
              \when checking that the expression ⋆ has type true ＝ true\n\n\
              \Reloaded to resync:\n\n\
              \Load succeeded: 2 goals.\n\n\
              \?0 : false ＝ false (at 75:29-34)\n\n\
              \?1 : true ＝ true (at 76:27-32)"
    , testCase "later give rejected with warnings and earlier gives discarded" $
        renderGiveResponse
          ( GiveResponse
              ( GiveRejected
                  ( GiveRejection
                      (InteractionId 3)
                      (Just (Span (Position 20 40 7) (Position 25 40 12)))
                      ( AgdaError
                          "1.1-5: error: Not in scope: nope"
                          Nothing
                          [ Warning (Nothing, "First warning")
                          , Warning (Nothing, "Second warning")
                          ]
                      )
                      (BatchPosition 1 2)
                  )
              )
              ( Loaded
                  []
                  []
                  [Warning (Nothing, "Reload warning")]
                  []
              )
          )
          @?= "Give rejected for ?3 (at 40:7-12; give 2 of 2). No file changes were made.\n\n\
              \Expression error (locations are relative to the submitted expression):\n\n\
              \1.1-5: error: Not in scope: nope\n\n\
              \Warnings:\n\n\
              \First warning\n\
              \Second warning\n\n\
              \Reloaded to resync:\n\n\
              \Load succeeded: no goals, 1 warning.\n\n\
              \Warnings:\n\n\
              \Reload warning"
    , testCase "reload failure after a rejection renders in full" $
        renderGiveResponse
          ( GiveResponse
              ( GiveRejected
                  ( GiveRejection
                      (InteractionId 0)
                      Nothing
                      ( AgdaError
                          "1.1-5: error: Not in scope: nope"
                          Nothing
                          []
                      )
                      (BatchPosition 0 1)
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
          @?= "Give rejected for ?0 (give 1 of 1). No file changes were made.\n\n\
              \Expression error (locations are relative to the submitted expression):\n\n\
              \1.1-5: error: Not in scope: nope\n\n\
              \Reloaded to resync:\n\n\
              \Load failed:\n\n\
              \Example.agda:3,1-4\n\
              \Not in scope: bad"
    , testCase "rejection reports its position in the batch" $
        renderGiveResponse
          ( GiveResponse
              ( GiveRejected
                  ( GiveRejection
                      (InteractionId 2)
                      Nothing
                      ( AgdaError
                          "1.1-5: error: Not in scope: nope"
                          Nothing
                          []
                      )
                      (BatchPosition 1 4)
                  )
              )
              (Loaded [] [] [] [])
          )
          @?= "Give rejected for ?2 (give 2 of 4). No file changes were made.\n\n\
              \Expression error (locations are relative to the submitted expression):\n\n\
              \1.1-5: error: Not in scope: nope\n\n\
              \Reloaded to resync:\n\n\
              \Load succeeded: no goals."
    ]

unknownGoalTests :: TestTree
unknownGoalTests =
  testGroup
    "unknown goals"
    [ testCase "unknown goal followed by the fresh goal list" $
        renderGiveResponse
          ( GiveResponse
              (GiveUnknownGoal (InteractionId 9) (BatchPosition 0 1))
              ( Loaded
                  [ Goal
                      (InteractionId 0)
                      (Span (Position 0 75 29) (Position 5 75 34))
                      (GoalOfType "false ＝ false")
                      []
                  ]
                  []
                  []
                  []
              )
          )
          @?= "No such goal ?9 (give 1 of 1). No file changes were made. Goal \
              \IDs renumber after every edit or reload; use the IDs from the \
              \fresh list below.\n\n\
              \Reloaded to resync:\n\n\
              \Load succeeded: 1 goal.\n\n\
              \?0 : false ＝ false (at 75:29-34)"
    , testCase "unknown goal later in the batch" $
        renderGiveResponse
          ( GiveResponse
              (GiveUnknownGoal (InteractionId 99) (BatchPosition 2 3))
              (Loaded [] [] [] [])
          )
          @?= "No such goal ?99 (give 3 of 3). No file changes were made. Goal \
              \IDs renumber after every edit or reload; use the IDs from the \
              \fresh list below.\n\n\
              \Reloaded to resync:\n\n\
              \Load succeeded: no goals."
    , testCase "unknown goal early in the batch" $
        renderGiveResponse
          ( GiveResponse
              (GiveUnknownGoal (InteractionId 5) (BatchPosition 0 4))
              (Loaded [] [] [] [])
          )
          @?= "No such goal ?5 (give 1 of 4). No file changes were made. Goal \
              \IDs renumber after every edit or reload; use the IDs from the \
              \fresh list below.\n\n\
              \Reloaded to resync:\n\n\
              \Load succeeded: no goals."
    ]

notLoadedTests :: TestTree
notLoadedTests =
  testGroup
    "not-loaded files"
    [ testCase "give against an unloaded file refused and the file loaded" $
        renderGiveResponse
          ( GiveResponse
              GiveNotLoaded
              ( Loaded
                  [ Goal
                      (InteractionId 0)
                      (Span (Position 0 75 29) (Position 5 75 34))
                      (GoalOfType "false ＝ false")
                      []
                  ]
                  []
                  []
                  []
              )
          )
          @?= "Give refused: the file is not the currently loaded file, and \
              \goal interaction IDs are only valid for the most recently \
              \loaded file. Nothing was checked and no changes were made. \
              \Loaded the file; use the goal IDs from the fresh result \
              \below:\n\n\
              \Load succeeded: 1 goal.\n\n\
              \?0 : false ＝ false (at 75:29-34)"
    , testCase "give against an unloaded file that fails to load" $
        renderGiveResponse
          ( GiveResponse
              GiveNotLoaded
              ( LoadFailed
                  ( AgdaError
                      "Example.agda:3,1-4\nNot in scope: bad"
                      (Just (Span (Position 10 3 1) (Position 13 3 4)))
                      []
                  )
              )
          )
          @?= "Give refused: the file is not the currently loaded file, and \
              \goal interaction IDs are only valid for the most recently \
              \loaded file. Nothing was checked and no changes were made. \
              \Loaded the file; use the goal IDs from the fresh result \
              \below:\n\n\
              \Load failed:\n\n\
              \Example.agda:3,1-4\n\
              \Not in scope: bad"
    ]

staleTests :: TestTree
staleTests =
  testGroup
    "stale edits"
    [ testCase "changed file refused and reloaded with a shifted goal" $
        renderGiveResponse
          ( GiveResponse
              GiveFileChanged
              ( Loaded
                  [ Goal
                      (InteractionId 0)
                      (Span (Position 101 76 29) (Position 106 76 34))
                      (GoalOfType "false ＝ false")
                      []
                  ]
                  []
                  []
                  []
              )
          )
          @?= "Edits refused: the file on disk is not the version Agda loaded (it changed since the last load). No changes were made.\n\n\
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
          @?= "The give could not be completed because the source file could not be accessed:\n\n\
              \Example.agda: permission denied\n\
              \while opening the file\n\n\
              \No changes were written.\n\n\
              \Reloaded to resync:\n\n\
              \Load failed:\n\n\
              \Cannot read file Example.agda"
    ]

parseArgumentTests :: TestTree
parseArgumentTests =
  testGroup
    "parseArguments"
    [ testCase "valid request" $
        case parseGive
          [ ("path", "/tmp/Hole.agda")
          , ("gives", toJSON [giveItem (toJSON (0 :: Int)) "y"])
          ] of
          Right (GiveRequest path items) -> do
            path @?= "/tmp/Hole.agda"
            items @?= [(InteractionId 0, "y")]
          Left message ->
            assertFailure ("unexpected parse failure: " <> Text.unpack message)
    , testCase "non-string path" $
        expectParseFailure "$.path" $
          parseGive
            [ ("path", toJSON (42 :: Int))
            , ("gives", toJSON [giveItem (toJSON (0 :: Int)) "y"])
            ]
    , testCase "empty gives" $
        expectParseFailure "at least one" $
          parseGive [("path", "/tmp/Hole.agda"), ("gives", toJSON ([] :: [Value]))]
    , testCase "string goal carries the element's path" $
        expectParseFailure "$.gives[0].goal" $
          parseGive
            [ ("path", "/tmp/Hole.agda")
            , ("gives", toJSON [giveItem "0" "y"])
            ]
    , testCase "non-integral goal carries the element's path" $
        expectParseFailure "$.gives[0].goal" $
          parseGive
            [ ("path", "/tmp/Hole.agda")
            , ("gives", toJSON [giveItem (toJSON (1.5 :: Double)) "y"])
            ]
    , testCase "duplicate goals" $
        expectParseFailure "Duplicate goal" $
          parseGive
            [ ("path", "/tmp/Hole.agda")
            ,
              ( "gives"
              , toJSON
                  [ giveItem (toJSON (0 :: Int)) "y"
                  , giveItem (toJSON (0 :: Int)) "zero"
                  ]
              )
            ]
    , testCase "blank expression carries the element's path" $
        expectParseFailure "$.gives[1]" $
          parseGive
            [ ("path", "/tmp/Hole.agda")
            ,
              ( "gives"
              , toJSON
                  [ giveItem (toJSON (0 :: Int)) "y"
                  , giveItem (toJSON (1 :: Int)) "  \n "
                  ]
              )
            ]
    ]

parseGive :: [(Text, Value)] -> Either Text GiveRequest
parseGive = parseArguments . Just . Map.fromList

giveItem :: Value -> Text -> Value
giveItem goal expression =
  object ["goal" .= goal, "expression" .= expression]

expectParseFailure :: Text -> Either Text GiveRequest -> IO ()
expectParseFailure fragment (Left message) =
  assertBool
    ( "expected the failure message to mention "
        <> Text.unpack fragment
        <> ", got: "
        <> Text.unpack message
    )
    (fragment `Text.isInfixOf` message)
expectParseFailure _ (Right _) =
  assertFailure "expected a parse failure, got a parsed request"
