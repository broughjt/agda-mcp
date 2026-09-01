{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Infer (tests) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Syntax.Common (InteractionId)
import Agda.TypeChecking.Monad (TCState)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tool.Harness (loadId, withFixtureToolSession)

import AgdaMCP.Interaction (Error (..), Warning (..))
import AgdaMCP.Interaction.GoalInfer (Have (..))
import AgdaMCP.Tools.Infer (
  InferReport (..),
  Request (..),
  Response (..),
  renderResponse,
 )
import AgdaMCP.Tools.Infer qualified as Infer
import AgdaMCP.Tools.Load (LoadRefusal (..))
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadId, LoadIdRefusal (..))
import AgdaMCP.Tools.MCP (parseArguments)
import AgdaMCP.Tools.Source (SourceRefusal (..))

tests :: IO TCState -> TestTree
tests warm =
  testGroup
    "Infer"
    [ parseArgumentsTests
    , renderResponseTests
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
    , testCase "an expression that does not infer blames the expression" $
        renderResponse (ResponseFailed 0 (plainError parseError))
          @?= Right
            ( rendered
                [ "Could not infer the type of the expression at ?0:"
                , ""
                , "  1.5: error: [ParseError]"
                , "  <EOF><ERROR> ..."
                ]
            )
    , testCase "the failure headline carries the queried goal id" $
        renderResponse (ResponseFailed 3 (plainError "it does not infer"))
          @?= Right
            ( rendered
                [ "Could not infer the type of the expression at ?3:"
                , ""
                , "  it does not infer"
                ]
            )
    , testCase "a failure's warnings follow under their own heading" $
        renderResponse
          ( ResponseFailed
              0
              (plainError "it does not infer")
                { errorWarnings =
                    [Warning (Nothing, "a warning raised on the way")]
                }
          )
          @?= Right
            ( rendered
                [ "Could not infer the type of the expression at ?0:"
                , ""
                , "  it does not infer"
                , ""
                , "Warnings:"
                , "  a warning raised on the way"
                ]
            )
    , testCase "an inferred type is reported with the expression it came from" $
        renderResponse (ResponseOk 0 inferredNatural)
          @?= Right
            ( rendered
                [ "Have at ?0:"
                , "  suc zero"
                , "    : ℕ"
                ]
            )
    , testCase "the section title carries the queried goal id" $
        renderResponse (ResponseOk 3 inferredNatural)
          @?= Right
            ( rendered
                [ "Have at ?3:"
                , "  suc zero"
                , "    : ℕ"
                ]
            )
    , testCase "a multi-line expression is echoed verbatim" $
        renderResponse
          ( ResponseOk
              0
              inferredNatural
                { inferReportExpression = "λ x\n  → x"
                , inferReportHave = Have "A" []
                }
          )
          @?= Right
            ( rendered
                [ "Have at ?0:"
                , "  λ x"
                , "    → x"
                , "    : A"
                ]
            )
    , testCase "a multi-line inferred type keeps Agda's own indentation" $
        renderResponse
          (ResponseOk 0 inferredNatural {inferReportHave = Have "A\n  → B" []})
          @?= Right
            ( rendered
                [ "Have at ?0:"
                , "  suc zero"
                , "    : A"
                , "      → B"
                ]
            )
    , testCase "the two boundaries are reported as separate sections" $
        renderResponse (ResponseOk 0 inferredPath)
          @?= Right
            ( rendered
                [ "Have at ?0:"
                , "  p i"
                , "    : A"
                , ""
                , "Boundary expected by ?0:"
                , "  i = i0 ⊢ y"
                , ""
                , "  i = i1 ⊢ x"
                , ""
                , "Boundary of the expression:"
                , "  i = i0 ⊢ x"
                , ""
                , "  i = i1 ⊢ y"
                ]
            )
    , testCase "boundaries that agree are still reported twice" $
        renderResponse
          ( ResponseOk
              0
              inferredPath {inferReportHave = Have "A" goalFaces}
          )
          @?= Right
            ( rendered
                [ "Have at ?0:"
                , "  p i"
                , "    : A"
                , ""
                , "Boundary expected by ?0:"
                , "  i = i0 ⊢ y"
                , ""
                , "  i = i1 ⊢ x"
                , ""
                , "Boundary of the expression:"
                , "  i = i0 ⊢ y"
                , ""
                , "  i = i1 ⊢ x"
                ]
            )
    , testCase "a goal boundary is reported when the expression has none" $
        renderResponse
          (ResponseOk 0 inferredPath {inferReportHave = Have "A" []})
          @?= Right
            ( rendered
                [ "Have at ?0:"
                , "  p i"
                , "    : A"
                , ""
                , "Boundary expected by ?0:"
                , "  i = i0 ⊢ y"
                , ""
                , "  i = i1 ⊢ x"
                ]
            )
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
          response <- Infer.infer (inferRequest (loadId 1) 5 "suc (")
          liftIO $ response @?= ResponseUnknownGoal 5
    , testCase "a parse error is a failure carrying expression-relative positions" $
        withFixtureToolSession warm "test/fixtures/InferCheck.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          response <- Infer.infer (inferRequest (loadId 1) 0 "suc (")
          liftIO $ case response of
            ResponseFailed goalId e -> do
              goalId @?= 0
              errorMessage e @?= parseError
            other ->
              assertFailure $
                "expected the unparseable expression to fail, got " <> show other
    , testCase "inference does not consult the goal type" $
        withFixtureToolSession warm "test/fixtures/InferCheck.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          report <-
            Infer.infer (inferRequest (loadId 1) 0 "tt")
              >>= liftIO . expectInferOk "the expression that only infers"
          liftIO $ do
            inferReportHave report @?= Have "⊤" []
            inferReportGoalBoundary report @?= []
    , testCase "the expression's boundary is reported beside the goal's" $
        withFixtureToolSession warm "test/fixtures/CubicalBoundary.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          report <-
            Infer.infer (inferRequest (loadId 1) 0 "p i")
              >>= liftIO . expectInferOk "the reversed path"
          liftIO $ do
            inferReportHave report @?= Have "A" ["i = i0 ⊢ x", "i = i1 ⊢ y"]
            inferReportGoalBoundary report @?= goalFaces
    , testCase "an expression fitting the boundary reports the goal's own faces" $
        withFixtureToolSession warm "test/fixtures/CubicalBoundary.agda" $ \path -> do
          _ <-
            Load.load Load.Request {Load.loadRequestPath = path}
              >>= liftIO . expectLoadOk "load"
          report <-
            Infer.infer (inferRequest (loadId 1) 0 "p (primINeg i)")
              >>= liftIO . expectInferOk "the symmetric path"
          liftIO $ do
            inferReportHave report @?= Have "A" goalFaces
            inferReportGoalBoundary report @?= goalFaces
    ]

-- Helpers

rendered :: [Text] -> Text
rendered = Text.intercalate "\n"

inferRequest :: LoadId -> InteractionId -> Text -> Request
inferRequest submitted goalId expression =
  Request
    { inferRequestLoadId = submitted
    , inferRequestGoalId = goalId
    , inferRequestNormalization = Simplified
    , inferRequestExpression = expression
    }

inferredNatural :: InferReport
inferredNatural =
  InferReport
    { inferReportExpression = "suc zero"
    , inferReportHave = Have "ℕ" []
    , inferReportGoalBoundary = []
    }

inferredPath :: InferReport
inferredPath =
  InferReport
    { inferReportExpression = "p i"
    , inferReportHave = Have "A" ["i = i0 ⊢ x", "i = i1 ⊢ y"]
    , inferReportGoalBoundary = goalFaces
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

-- Observed against CubicalBoundary.agda's ?0 : A.

goalFaces :: [Text]
goalFaces = ["i = i0 ⊢ y", "i = i1 ⊢ x"]

expectLoadOk :: String -> Load.Response -> IO Load.LoadReport
expectLoadOk _ (Load.ResponseOk report) = pure report
expectLoadOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectInferOk :: String -> Infer.Response -> IO InferReport
expectInferOk _ (ResponseOk _ report) = pure report
expectInferOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other
