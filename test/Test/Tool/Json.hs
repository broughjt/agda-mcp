{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Json (tests) where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AgdaMCP.Interaction (
  ContextEntry (..),
  Error (..),
  GoalShape (..),
  HiddenMetavariable (..),
  NonFatalError (..),
  Position (..),
  Span (..),
  Warning (..),
 )
import AgdaMCP.Tools.Json (
  encodeContextEntry,
  encodeError,
  encodeGoalId,
  encodeHiddenMetavariable,
  encodeLoadIdRefusal,
  encodeNonFatalError,
  encodePosition,
  encodeShape,
  encodeSourceRefusal,
  encodeSpan,
  encodeUnknownGoal,
  encodeWarning,
 )
import AgdaMCP.Tools.LoadId (LoadIdRefusal (..))
import AgdaMCP.Tools.Source (SourceRefusal (..))
import Test.Tool.Harness (loadId)

tests :: TestTree
tests =
  testGroup
    "Json"
    [ modelTests
    , refusalTests
    ]

modelTests :: TestTree
modelTests =
  testGroup
    "model"
    [ testCase "a goal id is a bare integer" $
        encodeGoalId 5 @?= Aeson.Number 5
    , testCase "a position carries offset, line and column" $
        encodePosition (Position 12 3 5)
          @?= Aeson.object
            [ "offset" Aeson..= (12 :: Int)
            , "line" Aeson..= (3 :: Int)
            , "column" Aeson..= (5 :: Int)
            ]
    , testCase "a span is its start and end" $
        encodeSpan (Span (Position 12 3 5) (Position 20 4 2))
          @?= Aeson.object
            [ "start" Aeson..= position 12 3 5
            , "end" Aeson..= position 20 4 2
            ]
    , testCase "a goal of a type" $
        encodeShape (GoalOfType "ℕ")
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "of_type"
            , "type" Aeson..= Aeson.String "ℕ"
            ]
    , testCase "a goal that is a sort" $
        encodeShape GoalSort @?= Aeson.object ["kind" Aeson..= Aeson.String "sort"]
    , testCase "a plain binding has every field, with nulls where nothing applies" $
        encodeContextEntry (contextEntry "x" "ℕ")
          @?= Aeson.object
            [ "original_name" Aeson..= Aeson.String "x"
            , "reified_name" Aeson..= Aeson.String "x"
            , "original_in_scope" Aeson..= True
            , "reified_in_scope" Aeson..= True
            , "type" Aeson..= Aeson.String "ℕ"
            , "let_value" Aeson..= Aeson.Null
            , "is_instance" Aeson..= False
            , "cohesion" Aeson..= Aeson.Null
            , "polarity" Aeson..= Aeson.Null
            , "erased" Aeson..= False
            , "relevance" Aeson..= Aeson.Null
            ]
    , testCase "a binding with every attribute" $
        encodeContextEntry
          (contextEntry "x" "A")
            { contextEntryReifiedName = "x₁"
            , contextEntryOriginalInScope = False
            , contextEntryReifiedInScope = False
            , contextEntryLetValue = Just "zero"
            , contextEntryIsInstance = True
            , contextEntryCohesion = Just "@♭"
            , contextEntryPolarity = Just "positive"
            , contextEntryErased = True
            , contextEntryRelevance = Just "irrelevant"
            }
          @?= Aeson.object
            [ "original_name" Aeson..= Aeson.String "x"
            , "reified_name" Aeson..= Aeson.String "x₁"
            , "original_in_scope" Aeson..= False
            , "reified_in_scope" Aeson..= False
            , "type" Aeson..= Aeson.String "A"
            , "let_value" Aeson..= Aeson.String "zero"
            , "is_instance" Aeson..= True
            , "cohesion" Aeson..= Aeson.String "@♭"
            , "polarity" Aeson..= Aeson.String "positive"
            , "erased" Aeson..= True
            , "relevance" Aeson..= Aeson.String "irrelevant"
            ]
    , testCase "an error without a location" $
        encodeError (plainError "it failed")
          @?= Aeson.object
            [ "message" Aeson..= Aeson.String "it failed"
            , "location" Aeson..= Aeson.Null
            , "warnings" Aeson..= ([] :: [Aeson.Value])
            ]
    , testCase "an error with a location and a warning" $
        encodeError
          (plainError "it failed")
            { errorPathSpan = Just ("/tmp/Example.agda", hole)
            , errorWarnings = [Warning (Nothing, "a warning")]
            }
          @?= Aeson.object
            [ "message" Aeson..= Aeson.String "it failed"
            , "location" Aeson..= location "/tmp/Example.agda" hole
            , "warnings"
                Aeson..= [ Aeson.object
                             [ "message" Aeson..= Aeson.String "a warning"
                             , "location" Aeson..= Aeson.Null
                             ]
                         ]
            ]
    , testCase "a warning with a location" $
        encodeWarning (Warning (Just ("/tmp/Example.agda", hole), "a warning"))
          @?= Aeson.object
            [ "message" Aeson..= Aeson.String "a warning"
            , "location" Aeson..= location "/tmp/Example.agda" hole
            ]
    , testCase "a non-fatal error without a location" $
        encodeNonFatalError (NonFatalError (Nothing, "unsolved constraints"))
          @?= Aeson.object
            [ "message" Aeson..= Aeson.String "unsolved constraints"
            , "location" Aeson..= Aeson.Null
            ]
    , testCase "a hidden metavariable with a span" $
        encodeHiddenMetavariable
          HiddenMetavariable
            { hiddenMetavariableName = "_12"
            , hiddenMetavariableSpan = Just hole
            , hiddenMetavariableShape = GoalOfType "ℕ"
            }
          @?= Aeson.object
            [ "name" Aeson..= Aeson.String "_12"
            , "span" Aeson..= encodeSpan hole
            , "shape"
                Aeson..= Aeson.object
                  [ "kind" Aeson..= Aeson.String "of_type"
                  , "type" Aeson..= Aeson.String "ℕ"
                  ]
            ]
    , testCase "a hidden metavariable without a span" $
        encodeHiddenMetavariable
          HiddenMetavariable
            { hiddenMetavariableName = "_14"
            , hiddenMetavariableSpan = Nothing
            , hiddenMetavariableShape = GoalSort
            }
          @?= Aeson.object
            [ "name" Aeson..= Aeson.String "_14"
            , "span" Aeson..= Aeson.Null
            , "shape" Aeson..= Aeson.object ["kind" Aeson..= Aeson.String "sort"]
            ]
    ]

refusalTests :: TestTree
refusalTests =
  testGroup
    "refusals"
    [ testCase "no current load" $
        encodeLoadIdRefusal NoCurrentLoad
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "no_current_load"
            , "message"
                Aeson..= Aeson.String
                  "No load is current. Either no file has been loaded yet, or the \
                  \most recent load failed. Load the file, then use the load ID and \
                  \goal IDs from that load result."
            ]
    , testCase "a stale load id names the current one" $
        encodeLoadIdRefusal (StaleLoadId $ loadId 2)
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
    , testCase "a foreign load id" $
        encodeLoadIdRefusal ForeignLoadId
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "foreign_load_id"
            , "message"
                Aeson..= Aeson.String
                  "The supplied load ID is from a session that no longer exists. \
                  \Load the file again and use the load ID from that result."
            ]
    , testCase "a source that changed" $
        encodeSourceRefusal RefusalChanged
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "source_changed"
            , "message"
                Aeson..= Aeson.String
                  "The file on disk no longer matches the source Agda checked. Load \
                  \the file again, then use the load ID and goal IDs from that load \
                  \result."
            ]
    , testCase "a source that could not be read carries the detail" $
        encodeSourceRefusal (RefusalUnreadable "no such file")
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "source_unreadable"
            , "detail" Aeson..= Aeson.String "no such file"
            , "message"
                Aeson..= Aeson.String
                  "Could not read the file to check it still matches the \
                  \source Agda checked. Load the file again once it is \
                  \readable, then use the load ID and goal IDs from that load \
                  \result.\n\n  no such file"
            ]
    , testCase "an unknown goal names the goal" $
        encodeUnknownGoal 5
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "unknown_goal"
            , "goal" Aeson..= (5 :: Int)
            , "message"
                Aeson..= Aeson.String
                  "No goal ?5 in the current load. Check the goal IDs in the most \
                  \recent result."
            ]
    ]

-- Helpers

hole :: Span
hole = Span (Position 10 2 5) (Position 11 2 6)

position :: Int -> Int -> Int -> Aeson.Value
position offset line column =
  Aeson.object
    [ "offset" Aeson..= offset
    , "line" Aeson..= line
    , "column" Aeson..= column
    ]

location :: FilePath -> Span -> Aeson.Value
location path s =
  Aeson.object ["path" Aeson..= path, "span" Aeson..= encodeSpan s]

plainError :: Text -> Error
plainError message =
  Error
    { errorMessage = message
    , errorPathSpan = Nothing
    , errorWarnings = []
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
