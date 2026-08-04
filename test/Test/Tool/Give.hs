{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Give (tests) where

import Agda.Syntax.Common (InteractionId)
import Agda.TypeChecking.Monad (TCState)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import AgdaMCP.Interaction (
  Error (..),
  Goal (..),
  GoalShape (..),
  Position (..),
  Span (..),
  Warning (..),
 )
import AgdaMCP.Tools.Give (
  Action (..),
  BatchPosition (..),
  Edit (..),
  EditKind (..),
  Outcome (..),
  Refusal (..),
  RefusalReason (..),
  Request (..),
  Response (..),
  give,
  renderResponse,
 )
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadId (..), LoadIdRefusal (..))
import AgdaMCP.Tools.MCP (parseArguments)
import AgdaMCP.Tools.State (ToolM)
import Data.Functor (void)
import Test.Corpus qualified as Corpus
import Test.Tool.Harness (withFixtureToolSession)

tests :: IO TCState -> TestTree
tests warm =
  testGroup
    "Give"
    [ parseArgumentsTests
    , renderResponseTests
    , sessionTests warm
    ]

parseArgumentsTests :: TestTree
parseArgumentsTests =
  testGroup
    "parseArguments"
    [ testCase "a call without any gives is refused" $
        parseRequest (Just $ Map.fromList [("load_id", Aeson.String "L1")])
          @?= Left "Error in $: key \"gives\" not found"
    , testCase "gives must be an array" $
        parseRequest
          ( argumentsValue $
              Aeson.object ["goal" Aeson..= (0 :: Int)]
          )
          @?= Left
            "Error in $.gives: parsing gives failed, expected Array, but \
            \encountered Object"
    , testCase "an empty batch is refused" $
        parseRequest (arguments [])
          @?= Left
            "Error in $.gives: expected at least one goal to fill, but got none"
    , testCase "an item must be an object" $
        parseRequest (arguments [Aeson.String "not an item"])
          @?= Left
            "Error in $.gives[0]: parsing give item failed, expected Object, \
            \but encountered String"
    , testCase "an item must include a goal" $
        parseRequest
          ( arguments
              [Aeson.object ["expression" Aeson..= ("refl" :: Text)]]
          )
          @?= Left "Error in $.gives[0]: key \"goal\" not found"
    , testCase "an item must include an expression" $
        parseRequest
          (arguments [Aeson.object ["goal" Aeson..= (0 :: Int)]])
          @?= Left "Error in $.gives[0]: key \"expression\" not found"
    , testCase "a goal must be integral" $
        parseRequest (arguments [itemValue (Aeson.Number 1.5) "refl" Nothing])
          @?= Left
            "Error in $.gives[0].goal: parsing Int failed, value is either \
            \floating or will cause over or underflow 1.5"
    , testCase "a goal must be non-negative" $
        parseRequest (arguments [item (-1) "refl" Nothing])
          @?= Left
            "Error in $.gives[0].goal: expected a non-negative goal ID"
    , testCase "a blank expression is refused" $
        parseRequest (arguments [item 0 "" Nothing])
          @?= Left
            "Error in $.gives[0].expression: expected an Agda expression to \
            \write into the goal, but got \"\""
    , testCase "a whitespace-only expression is refused" $
        parseRequest (arguments [item 0 "  " Nothing])
          @?= Left
            "Error in $.gives[0].expression: expected an Agda expression to \
            \write into the goal, but got \"  \""
    , testCase "a goal repeated within one batch is refused" $
        parseRequest
          (arguments [item 1 "refl" Nothing, item 1 "zero" Nothing])
          @?= Left
            "Error in $.gives: expected each goal to appear at most once, but \
            \?1 appears more than once"
    , testCase "the first repeated goal is reported" $
        parseRequest
          ( arguments
              [ item 9 "refl" Nothing
              , item 2 "zero" Nothing
              , item 9 "suc zero" Nothing
              , item 2 "suc (suc zero)" Nothing
              ]
          )
          @?= Left
            "Error in $.gives: expected each goal to appear at most once, but \
            \?9 appears more than once"
    , testCase "an unknown action is refused" $
        parseRequest (arguments [item 0 "refl" (Just "elaborate")])
          @?= Left "Error in $.gives[0].action: expected one of give, refine"
    , testCase "an action must be a string" $
        parseRequest
          ( arguments
              [ itemValue
                  (Aeson.toJSON (0 :: Int))
                  "refl"
                  (Just $ Aeson.Bool True)
              ]
          )
          @?= Left
            "Error in $.gives[0].action: parsing action failed, expected String, \
            \but encountered Boolean"
    , testCase "an explicit null action is refused" $
        parseRequest
          (arguments [itemValue (Aeson.toJSON (0 :: Int)) "refl" (Just Aeson.Null)])
          @?= Left
            "Error in $.gives[0].action: parsing action failed, expected String, \
            \but encountered Null"
    , testCase "the action defaults to give" $
        parseRequest (arguments [item 0 "refl" Nothing])
          @?= Right (Request (LoadId 1) [(0, ActionGive "refl")])
    , testCase "a batch of both actions parses in order" $
        parseRequest
          ( arguments
              [ item 0 "refl" (Just "give")
              , item 1 "suc ?" (Just "refine")
              ]
          )
          @?= Right
            ( Request
                (LoadId 1)
                [(0, ActionGive "refl"), (1, ActionRefine "suc ?")]
            )
    ]
 where
  parseRequest :: Maybe (Map.Map Text Aeson.Value) -> Either Text Request
  parseRequest = parseArguments

  arguments :: [Aeson.Value] -> Maybe (Map.Map Text Aeson.Value)
  arguments gives =
    argumentsValue (Aeson.toJSON gives)

  argumentsValue :: Aeson.Value -> Maybe (Map.Map Text Aeson.Value)
  argumentsValue gives =
    Just $
      Map.fromList
        [ ("load_id", Aeson.String "L1")
        , ("gives", gives)
        ]

  item :: Int -> Text -> Maybe Text -> Aeson.Value
  item goalId expression action =
    itemValue
      (Aeson.toJSON goalId)
      expression
      (Aeson.toJSON <$> action)

  itemValue :: Aeson.Value -> Text -> Maybe Aeson.Value -> Aeson.Value
  itemValue goalId expression action =
    Aeson.object $
      [ "goal" Aeson..= goalId
      , "expression" Aeson..= expression
      ]
        <> foldMap (\a -> ["action" Aeson..= a]) action

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
            \is L2. Each load makes fresh goal ID assignments, so use the load \
            \ID and goal IDs from the most recent load result. If you no longer \
            \have that result, load the file again."
    , testCase "one applied edit reports the text that went in" $
        completed (OutcomeApplied [edit 0 "refl"])
          @?= Right
            ( rendered $
                [ "Applied 1 edit:"
                , "  ?0 = refl"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "several applied edits are listed in order" $
        completed (OutcomeApplied [edit 0 "refl", edit 1 "suc zero"])
          @?= Right
            ( rendered $
                [ "Applied 2 edits:"
                , "  ?0 = refl"
                , ""
                , "  ?1 = suc zero"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "an edit whose text wraps keeps Agda's own line break" $
        completed (OutcomeApplied [edit 1 "trans a\n(sym b)"])
          @?= Right
            ( rendered $
                [ "Applied 1 edit:"
                , "  ?1 = trans a"
                , "  (sym b)"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "a rejected batch reports Agda's error and writes nothing" $
        completed
          ( OutcomeRefused
              Refusal
                { refusalGoalId = 1
                , refusalSpan = Just holeSpan
                , refusalPosition = BatchPosition 1 2
                , refusalReason = RefusedError Corpus.typeError
                }
          )
          @?= Right
            ( rendered $
                [ "Rejected at ?1 (item 2 of 2). No edits were written."
                , ""
                , "  /fixture/TypeError.agda:6.9-10: error: [UnequalTerms]"
                , "  Set !=< ℕ"
                , "  when checking that the expression ℕ has type ℕ"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "a single-item batch does not report a batch position" $
        completed
          ( OutcomeRefused
              Refusal
                { refusalGoalId = 0
                , refusalSpan = Just holeSpan
                , refusalPosition = BatchPosition 0 1
                , refusalReason = RefusedError (plainError "it does not check")
                }
          )
          @?= Right
            ( rendered $
                [ "Rejected at ?0. No edits were written."
                , ""
                , "  it does not check"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "an unknown goal id is rejected without a span" $
        completed
          ( OutcomeRefused
              Refusal
                { refusalGoalId = 7
                , refusalSpan = Nothing
                , refusalPosition = BatchPosition 0 2
                , refusalReason = RefusedUnknownGoal
                }
          )
          @?= Right
            ( rendered $
                [ "Rejected at ?7 (item 1 of 2). No edits were written."
                , ""
                , "  There is no such goal in the current load. Check the goal \
                  \IDs in the most recent load result."
                , ""
                ]
                  <> reloadLines
            )
    , testCase "a rejection's warnings follow as further items" $
        completed
          ( OutcomeRefused
              Refusal
                { refusalGoalId = 0
                , refusalSpan = Just holeSpan
                , refusalPosition = BatchPosition 0 1
                , refusalReason =
                    RefusedError
                      (plainError "it does not check")
                        { errorWarnings =
                            [Warning (Nothing, "a warning raised on the way")]
                        }
                }
          )
          @?= Right
            ( rendered $
                [ "Rejected at ?0. No edits were written."
                , ""
                , "  it does not check"
                , ""
                , "Warnings:"
                , "  a warning raised on the way"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "a changed file is refused before anything is written" $
        completed OutcomeFileChanged
          @?= Right
            ( rendered $
                [ "The file on disk no longer matches the source Agda checked, \
                  \so no edits were written. It has been reloaded below; goal \
                  \IDs from the earlier load are no longer valid."
                , ""
                ]
                  <> reloadLines
            )
    , testCase "an unreadable file reports why it could not be read" $
        completed (OutcomeSourceUnreadable "Example.agda: openFile: does not exist")
          @?= Right
            ( rendered $
                [ "Could not read the file to check it still matches the source \
                  \Agda checked, so no edits were written."
                , ""
                , "  Example.agda: openFile: does not exist"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "a failed write reports that the file is unchanged" $
        completed (OutcomeWriteFailed "Example.agda: openFile: permission denied")
          @?= Right
            ( rendered $
                [ "Could not write the file. It was not modified."
                , ""
                , "  Example.agda: openFile: permission denied"
                , ""
                ]
                  <> reloadLines
            )
    , testCase "the reload is rendered exactly as the load tool renders it" $
        ResponseCompleted (OutcomeApplied [edit 0 "suc ?"]) reloadWithGoal
          `renders` ( rendered
                        [ "Applied 1 edit:"
                        , "  ?0 = suc ?"
                        , ""
                        , "Load succeeded: 1 goal"
                        , "Load ID: L5"
                        , "File: /tmp/Example.agda"
                        , ""
                        , "?0 at 14:16-17"
                        , "  ⊢ ℕ"
                        ]
                    )
    , testCase "a reload that failed is reported as the load tool reports it" $
        ResponseCompleted
          (OutcomeApplied [edit 0 "refl"])
          (Load.ResponseError Corpus.typeError)
          `renders` ( rendered
                        [ "Applied 1 edit:"
                        , "  ?0 = refl"
                        , ""
                        , "Load failed:"
                        , ""
                        , "  /fixture/TypeError.agda:6.9-10: error: [UnequalTerms]"
                        , "  Set !=< ℕ"
                        , "  when checking that the expression ℕ has type ℕ"
                        ]
                    )
    ]
 where
  renders response expected = renderResponse response @?= Right expected

sessionTests :: IO TCState -> TestTree
sessionTests warm =
  testGroup
    "sessions"
    [ testCase "a file edited on disk since the load is refused unwritten" $
        withFixtureToolSession warm "test/fixtures/HoleNatural.agda" $ \path -> do
          void $ expectLoaded (LoadId 1) path
          liftIO $ Text.IO.appendFile path onDiskEdit
          (outcome, report) <-
            give (Request (LoadId 1) [(0, ActionGive "zero")])
              >>= liftIO . expectCompleted "a give against an edited file"
          liftIO $ do
            outcome @?= OutcomeFileChanged
            contentsShouldBe path (holeNatural <> onDiskEdit)
            Load.loadReportId report @?= LoadId 2
            goals report @?= [(0, GoalOfType "ℕ")]
    , testCase
        "when the file is edited on disk and the expression would be rejected, \
        \the edit is reported rather than the rejection"
        $ withFixtureToolSession warm "test/fixtures/GiveExpressions.agda"
        $ \path -> do
          _ <- expectLoaded (LoadId 1) path
          liftIO $ Text.IO.appendFile path onDiskEdit
          (outcome, _) <-
            give (Request (LoadId 1) [(0, ActionGive "suc suc")])
              >>= liftIO . expectCompleted "an ill-typed give against an edited file"
          liftIO $ outcome @?= OutcomeFileChanged
    , testCase "a literate file is spliced inside its code block" $
        withFixtureToolSession warm "test/fixtures/Literate.lagda.md" $ \path -> do
          void $ expectLoaded (LoadId 1) path
          (outcome, report) <-
            give (Request (LoadId 1) [(0, ActionGive "suc zero")])
              >>= liftIO . expectCompleted "a give into a literate hole"
          liftIO $ do
            case outcome of
              OutcomeApplied [applied] -> do
                editGoalId applied @?= 0
                editText applied @?= "suc zero"
                editKind applied @?= EditKindVerbatim
              other -> assertFailure $ "unexpected outcome: " <> show other
            contentsShouldBe path literateFilled
            Load.loadReportId report @?= LoadId 2
            goals report @?= []
    , testCase "the holes a refine leaves behind arrive in its reload" $
        withFixtureToolSession warm "test/fixtures/RefineExpressions.agda" $ \path -> do
          before <- expectLoaded (LoadId 1) path
          (outcome, report) <-
            give (Request (LoadId 1) [(1, ActionRefine "_+_")])
              >>= liftIO . expectCompleted "a refine missing two arguments"
          liftIO $ do
            case outcome of
              OutcomeApplied [applied] -> do
                editGoalId applied @?= 1
                editText applied @?= "? + ?"
                editKind applied @?= EditKindComputed
              other -> assertFailure $ "unexpected outcome: " <> show other
            lineShouldBe path 10 "twoHoles = ? + ?"
            goalShapes before @?= replicate 3 (GoalOfType "ℕ")
            Load.loadReportId report @?= LoadId 2
            goalShapes report @?= replicate 4 (GoalOfType "ℕ")
    , testCase "a rejected batch is reloaded and issues the next load id" $
        withFixtureToolSession warm "test/fixtures/GiveExpressions.agda" $ \path -> do
          before <- expectLoaded (LoadId 1) path
          contents <- liftIO $ Text.IO.readFile path
          (outcome, report) <-
            give (Request (LoadId 1) [(0, ActionGive "suc suc")])
              >>= liftIO . expectCompleted "an ill-typed give"
          liftIO $ do
            case outcome of
              OutcomeRefused refusal -> do
                refusalGoalId refusal @?= 0
                refusalPosition refusal @?= BatchPosition 0 1
                case refusalReason refusal of
                  RefusedError e ->
                    assertBool
                      ("expected \"UnequalTerms\" within " <> show (errorMessage e))
                      ("UnequalTerms" `Text.isInfixOf` errorMessage e)
                  other -> assertFailure $ "unexpected reason: " <> show other
              other -> assertFailure $ "unexpected outcome: " <> show other
            contentsShouldBe path contents
            Load.loadReportId report @?= LoadId 2
            goals report @?= goals before
    , testCase
        "a goal id that is not in the load is reloaded and issues the next load id"
        $ withFixtureToolSession warm "test/fixtures/GiveExpressions.agda"
        $ \path -> do
          _ <- expectLoaded (LoadId 1) path
          contents <- liftIO $ Text.IO.readFile path
          (outcome, report) <-
            give (Request (LoadId 1) [(99, ActionGive "zero")])
              >>= liftIO . expectCompleted "a give at a goal that does not exist"
          liftIO $ do
            outcome
              @?= OutcomeRefused
                Refusal
                  { refusalGoalId = 99
                  , refusalSpan = Nothing
                  , refusalPosition = BatchPosition 0 1
                  , refusalReason = RefusedUnknownGoal
                  }
            contentsShouldBe path contents
            Load.loadReportId report @?= LoadId 2
    ]

-- Helpers

expectLoaded :: LoadId -> FilePath -> ToolM Load.LoadReport
expectLoaded loadId path = do
  report <-
    Load.load Load.Request {Load.loadRequestPath = path}
      >>= liftIO . expectLoadOk "load"
  liftIO $ Load.loadReportId report @?= loadId
  pure report

expectLoadOk :: String -> Load.Response -> IO Load.LoadReport
expectLoadOk _ (Load.ResponseOk report) = pure report
expectLoadOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectCompleted :: String -> Response -> IO (Outcome, Load.LoadReport)
expectCompleted label (ResponseCompleted outcome resync) =
  (,) outcome <$> expectLoadOk (label <> "'s resync") resync
expectCompleted label other =
  assertFailure $ label <> ": expected ResponseCompleted, got " <> show other

goals :: Load.LoadReport -> [(InteractionId, GoalShape)]
goals = map (\(g, _) -> (goalId g, goalShape g)) . Load.loadReportGoals

goalShapes :: Load.LoadReport -> [GoalShape]
goalShapes = map snd . goals

contentsShouldBe :: FilePath -> Text -> IO ()
contentsShouldBe path expected = do
  contents <- Text.IO.readFile path
  contents @?= expected

lineShouldBe :: FilePath -> Int -> Text -> IO ()
lineShouldBe path number expected = do
  contents <- Text.IO.readFile path
  Text.lines contents !! (number - 1) @?= expected

onDiskEdit :: Text
onDiskEdit = "\n-- edited on disk\n"

holeNatural :: Text
holeNatural =
  Text.unlines
    [ "module HoleNatural where"
    , ""
    , "open import Data.Nat"
    , ""
    , "foo : ℕ → ℕ"
    , "foo n = {!!}"
    ]

literateFilled :: Text
literateFilled =
  Text.unlines
    [ "# Literate"
    , ""
    , "Prose before the code block."
    , ""
    , "```agda"
    , "module Literate where"
    , ""
    , "open import Data.Nat using (ℕ; zero; suc)"
    , ""
    , "two : ℕ"
    , "two = suc zero"
    , "```"
    , ""
    , "Prose after the code block."
    ]

rendered :: [Text] -> Text
rendered = Text.intercalate "\n"

completed :: Outcome -> Either Text Text
completed outcome = renderResponse (ResponseCompleted outcome reload)

reload :: Load.Response
reload = Load.ResponseOk reloadReport

reloadLines :: [Text]
reloadLines =
  [ "Load succeeded: no goals"
  , "Load ID: L5"
  , "File: /tmp/Example.agda"
  ]

reloadReport :: Load.LoadReport
reloadReport =
  Load.LoadReport
    { Load.loadReportId = LoadId 5
    , Load.loadReportPath = "/tmp/Example.agda"
    , Load.loadReportGoals = []
    , Load.loadReportHiddenMetavariables = []
    , Load.loadReportWarnings = []
    , Load.loadReportNonFatalErrors = []
    }

reloadWithGoal :: Load.Response
reloadWithGoal =
  Load.ResponseOk
    reloadReport
      { Load.loadReportGoals =
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

edit :: Int -> Text -> Edit
edit goalId text =
  Edit
    { editGoalId = fromIntegral goalId
    , editSpan = holeSpan
    , editText = text
    , editKind = EditKindComputed
    }

holeSpan :: Span
holeSpan = Span (Position 10 2 5) (Position 11 2 6)

plainError :: Text -> Error
plainError message =
  Error
    { errorMessage = message
    , errorPathSpan = Nothing
    , errorWarnings = []
    }
