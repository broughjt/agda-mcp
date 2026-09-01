{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Check (tests) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Syntax.Common (InteractionId)
import Agda.TypeChecking.Monad (TCState)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Directory (removeFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tool.Harness (describedKinds, loadId, withFixtureToolSession)

import AgdaMCP.Interaction (Error (..), Warning (..))
import AgdaMCP.Tools.Check (
  Request (..),
  Response (..),
  checkTool,
  renderResponse,
 )
import AgdaMCP.Tools.Check qualified as Check
import AgdaMCP.Tools.Load (LoadRefusal (..))
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadId, LoadIdRefusal (..))
import AgdaMCP.Tools.Source (SourceRefusal (..))
import AgdaMCP.Tools.Tool (parseArguments)

tests :: IO TCState -> TestTree
tests warm =
  testGroup
    "Check"
    [ parseArgumentsTests
    , renderResponseTests
    , toJSONTests
    , sessionTests warm
    ]

parseArgumentsTests :: TestTree
parseArgumentsTests =
  testGroup
    "parseArguments"
    [ testCase "a call without an expression is refused" $
        parseRequest
          ( Map.fromList
              [ ("load_id", Aeson.String "L1-k7x2")
              , ("goal", Aeson.Number 0)
              ]
          )
          @?= Left "Error in $: key \"expression\" not found"
    , testCase "a blank expression is refused" $
        parseRequest (arguments "")
          @?= Left
            "Error in $.expression: expected an Agda expression to try at the \
            \goal, but got \"\""
    , testCase "a whitespace-only expression is refused" $
        parseRequest (arguments "  ")
          @?= Left
            "Error in $.expression: expected an Agda expression to try at the \
            \goal, but got \"  \""
    , testCase "normalization defaults to simplified" $
        parseRequest (arguments "suc zero")
          @?= Right (Request (loadId 1) 0 Simplified "suc zero")
    , testCase "all four arguments parse" $
        parseRequest
          ( Map.fromList
              [ ("load_id", Aeson.String "L3-k7x2")
              , ("goal", Aeson.Number 2)
              , ("normalization", Aeson.String "normalized")
              , ("expression", Aeson.String "suc zero")
              ]
          )
          @?= Right (Request (loadId 3) 2 Normalised "suc zero")
    ]
 where
  parseRequest :: Map.Map Text Aeson.Value -> Either Text Request
  parseRequest = parseArguments
  arguments expression =
    Map.fromList
      [ ("load_id", Aeson.String "L1-k7x2")
      , ("goal", Aeson.Number 0)
      , ("expression", Aeson.String expression)
      ]

renderResponseTests :: TestTree
renderResponseTests =
  testGroup
    "renderResponse"
    [ testCase "a refusal when nothing is loaded says to load first" $
        renderResponse (ResponseRefused (RefusedLoadId NoCurrentLoad))
          @?= Left
            "No load is current. Either no file has been loaded yet, or the \
            \most recent load failed. Load the file, then use the load ID and \
            \goal IDs from that load result."
    , testCase "a stale load_id names the current load" $
        renderResponse (ResponseRefused $ RefusedLoadId $ StaleLoadId $ loadId 2)
          @?= Left
            "The supplied load ID is from an earlier load. The current load ID \
            \is L2-k7x2. Each load makes fresh goal ID assignments, so use the most \
            \recently issued load ID and goal IDs. If you no longer have them, \
            \load the file again."
    , testCase "a file that changed on disk says to load it again" $
        renderResponse (ResponseRefused $ RefusedSource RefusalChanged)
          @?= Left
            "The file on disk no longer matches the source Agda checked. Load \
            \the file again, then use the load ID and goal IDs from that load \
            \result."
    , testCase "an unknown goal id points back at the load result" $
        renderResponse (ResponseUnknownGoal 5)
          @?= Left
            "No goal ?5 in the current load. Check the goal IDs in the most \
            \recent result."
    , testCase "an expression that fits states the verdict and elaborates it" $
        renderResponse (ResponseOk 0 "1")
          @?= Right
            ( rendered
                [ "The expression checks at ?0."
                , ""
                , "Elaborates to:"
                , "  1"
                ]
            )
    , testCase "the verdict carries the queried goal id" $
        renderResponse (ResponseOk 3 "1")
          @?= Right
            ( rendered
                [ "The expression checks at ?3."
                , ""
                , "Elaborates to:"
                , "  1"
                ]
            )
    , testCase "a multi-line elaboration keeps Agda's own indentation" $
        renderResponse (ResponseOk 0 "λ x\n  → x")
          @?= Right
            ( rendered
                [ "The expression checks at ?0."
                , ""
                , "Elaborates to:"
                , "  λ x"
                , "    → x"
                ]
            )
    , testCase "an expression that does not fit reports the error, not an elaboration" $
        renderResponse (ResponseFailed 0 (plainError ttCheckError))
          @?= Right
            ( rendered
                [ "The expression does not check at ?0:"
                , ""
                , "  1.1-3: error: [UnequalTerms]"
                , "  ⊤ !=< ℕ"
                , "  when checking that the expression tt has type ℕ"
                ]
            )
    , testCase "the failure headline carries the queried goal id" $
        renderResponse (ResponseFailed 3 (plainError parseError))
          @?= Right
            ( rendered
                [ "The expression does not check at ?3:"
                , ""
                , "  1.5: error: [ParseError]"
                , "  <EOF><ERROR> ..."
                ]
            )
    , testCase "a failure's warnings follow under their own heading" $
        renderResponse
          ( ResponseFailed
              0
              (plainError ttCheckError)
                { errorWarnings =
                    [Warning (Nothing, "a warning raised on the way")]
                }
          )
          @?= Right
            ( rendered
                [ "The expression does not check at ?0:"
                , ""
                , "  1.1-3: error: [UnequalTerms]"
                , "  ⊤ !=< ℕ"
                , "  when checking that the expression tt has type ℕ"
                , ""
                , "Warnings:"
                , "  a warning raised on the way"
                ]
            )
    ]

toJSONTests :: TestTree
toJSONTests =
  testGroup
    "toJSON"
    [ testCase "a refusal is its own kind, with the prose as its message" $
        Aeson.toJSON (ResponseRefused (RefusedLoadId NoCurrentLoad))
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "no_current_load"
            , "message"
                Aeson..= Aeson.String
                  "No load is current. Either no file has been loaded yet, or the \
                  \most recent load failed. Load the file, then use the load ID and \
                  \goal IDs from that load result."
            ]
    , testCase "an unknown goal" $
        Aeson.toJSON (ResponseUnknownGoal 5)
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "unknown_goal"
            , "goal" Aeson..= (5 :: Int)
            , "message"
                Aeson..= Aeson.String
                  "No goal ?5 in the current load. Check the goal IDs in the most \
                  \recent result."
            ]
    , testCase "an expression that does not check carries the goal and the error" $
        Aeson.toJSON (ResponseFailed 3 (plainError parseError))
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "failed"
            , "goal" Aeson..= (3 :: Int)
            , "error"
                Aeson..= Aeson.object
                  [ "message" Aeson..= parseError
                  , "location" Aeson..= Aeson.Null
                  , "warnings" Aeson..= ([] :: [Aeson.Value])
                  ]
            ]
    , testCase "an expression that checks carries its elaboration" $
        Aeson.toJSON (ResponseOk 0 "1")
          @?= Aeson.object
            [ "kind" Aeson..= Aeson.String "checks"
            , "goal" Aeson..= (0 :: Int)
            , "elaborated" Aeson..= Aeson.String "1"
            ]
    , testCase "every kind is described" $
        describedKinds
          checkTool
          renderResponse
          [ ResponseRefused (RefusedLoadId NoCurrentLoad)
          , ResponseRefused (RefusedSource RefusalChanged)
          , ResponseUnknownGoal 5
          , ResponseFailed 3 (plainError parseError)
          , ResponseOk 0 "1"
          ]
    ]

sessionTests :: IO TCState -> TestTree
sessionTests warm =
  testGroup
    "sessions"
    [ testCase "an unknown goal id is answered before the expression is parsed" $
        withFixtureToolSession warm "test/fixtures/InferCheck.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          response <- Check.check (checkRequest (loadId 1) 5 "suc (")
          liftIO $ response @?= ResponseUnknownGoal 5
    , testCase "a parse error is a failure carrying expression-relative positions" $
        withFixtureToolSession warm "test/fixtures/InferCheck.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          response <- Check.check (checkRequest (loadId 1) 0 "suc (")
          liftIO $ case response of
            ResponseFailed goalId e -> do
              goalId @?= 0
              errorMessage e @?= parseError
            other ->
              assertFailure $
                "expected the unparseable expression to fail, got " <> show other
    , testCase "an expression that only infers is reported as a failure" $
        withFixtureToolSession warm "test/fixtures/InferCheck.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          response <- Check.check (checkRequest (loadId 1) 0 "tt")
          liftIO $ case response of
            ResponseFailed goalId e -> do
              goalId @?= 0
              errorMessage e @?= ttCheckError
            other ->
              assertFailure $
                "expected the ill-typed expression to fail, got " <> show other
    , testCase "checking does not verify a cubical goal's boundary" $
        withFixtureToolSession warm "test/fixtures/CubicalBoundary.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          response <- Check.check (checkRequest (loadId 1) 0 "p i")
          liftIO $ response @?= ResponseOk 0 "p i"
    , testCase "a file edited behind the session is refused until reloaded" $
        withFixtureToolSession warm "test/fixtures/InferCheck.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "the first load"
          liftIO $ Text.IO.appendFile path "\n-- edited in another window\n"
          refused <- Check.check (checkRequest (loadId 1) 0 "zero")
          liftIO $ refused @?= ResponseRefused (RefusedSource RefusalChanged)
          reloaded <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "the reload"
          liftIO $ Load.loadReportId reloaded @?= loadId 2
          answered <- Check.check (checkRequest (loadId 2) 0 "zero")
          liftIO $ answered @?= ResponseOk 0 "zero"
    , testCase "a file that has gone missing is refused" $
        withFixtureToolSession warm "test/fixtures/InferCheck.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          liftIO $ removeFile path
          refused <- Check.check (checkRequest (loadId 1) 0 "zero")
          liftIO $ case refused of
            ResponseRefused (RefusedSource (RefusalUnreadable _)) -> pure ()
            other ->
              assertFailure $
                "expected the missing file to be refused, got " <> show other
    ]

-- Helpers

rendered :: [Text] -> Text
rendered = Text.intercalate "\n"

checkRequest :: LoadId -> InteractionId -> Text -> Request
checkRequest submitted goalId expression =
  Request
    { checkRequestLoadId = submitted
    , checkRequestGoalId = goalId
    , checkRequestNormalization = Simplified
    , checkRequestExpression = expression
    }

plainError :: Text -> Error
plainError message =
  Error
    { errorMessage = message
    , errorPathSpan = Nothing
    , errorWarnings = []
    }

-- Observed against InferCheck.agda's ?0 : ℕ at Simplified.

parseError :: Text
parseError = "1.5: error: [ParseError]\n<EOF><ERROR> ..."

ttCheckError :: Text
ttCheckError =
  rendered
    [ "1.1-3: error: [UnequalTerms]"
    , "⊤ !=< ℕ"
    , "when checking that the expression tt has type ℕ"
    ]

expectLoadOk :: String -> Load.Response -> IO Load.LoadReport
expectLoadOk _ (Load.ResponseOk report) = pure report
expectLoadOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other
