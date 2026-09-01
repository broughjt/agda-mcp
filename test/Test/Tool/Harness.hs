{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Harness (
  fixtureNonce,
  loadId,
  withFixtureToolSession,
  withFixtureToolSessions,
  withFixtureToolState,
  describedKinds,
  describedOutcomes,
) where

import Agda.TypeChecking.Monad (TCState)
import Control.Monad (forM_)
import Control.Monad.State (runStateT)
import Data.Aeson (ToJSON, Value (..), toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)
import System.FilePath (takeFileName, (</>))
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

import AgdaMCP.Tools.Description (
  Description (..),
  Field (..),
  Shape (..),
  Variant (..),
 )
import AgdaMCP.Tools.LoadId (
  LoadGeneration (..),
  LoadId (..),
  SessionNonce (..),
 )
import AgdaMCP.Tools.State (ToolM, ToolState (..))
import AgdaMCP.Tools.Tool (Tool (..))
import Test.Harness (warmedSession, withStagedFiles)

{- | Fix a single session nonce so that tests can pin rendered load ids
literally.
-}
fixtureNonce :: SessionNonce
fixtureNonce = SessionNonce "k7x2"

-- | A load id from the fixture session.
loadId :: Natural -> LoadId
loadId = LoadId fixtureNonce

withFixtureToolSession ::
  IO TCState -> FilePath -> (FilePath -> ToolM a) -> IO a
withFixtureToolSession warm source k =
  withFixtureToolSessions warm [source] $ \paths ->
    case paths of
      [path] -> k path
      _ -> error "withFixtureToolSessions answered a different length"

withFixtureToolSessions ::
  IO TCState -> [FilePath] -> ([FilePath] -> ToolM a) -> IO a
withFixtureToolSessions warm sources k =
  withFixtureToolState warm sources $ \paths toolState ->
    fst <$> runStateT (k paths) toolState

withFixtureToolState ::
  IO TCState -> [FilePath] -> ([FilePath] -> ToolState -> IO a) -> IO a
withFixtureToolState warm sources k =
  withStagedFiles sources $ \directory options -> do
    interactionState <- warmedSession warm options
    k
      (map ((directory </>) . takeFileName) sources)
      ToolState
        { toolInteractionState = interactionState
        , toolLoadGeneration =
            LoadGeneration
              { sessionNonce = fixtureNonce
              , loadsIssued = 0
              , currentLoad = Nothing
              }
        }

{- | Each value's kind is among the tool's described answers when it renders
as an answer, and among its refusals when it renders as a refusal.
-}
describedKinds ::
  (ToJSON r) => Tool -> (r -> Either Text Text) -> [r] -> Assertion
describedKinds tool render = mapM_ check
 where
  description = toolDescription tool
  check r = do
    kind <- kindOf (toJSON r)
    let described =
          either
            (const $ descriptionRefusals description)
            (const $ descriptionAnswers description)
            (render r)
    assertBool
      (Text.unpack kind <> " is not a described kind")
      (kind `elem` map variantKind described)

{- | Each value's kind under the named field is among the variants of the
nested sum the tool describes there.
-}
describedOutcomes :: (ToJSON r) => Tool -> Text -> [r] -> Assertion
describedOutcomes tool name values = do
  variants <- case nestedSums of
    [variants] -> pure variants
    _ -> assertFailure $ "no nested sum at " <> Text.unpack name
  forM_ values $ \r -> do
    nested <- fieldOf name (toJSON r)
    kind <- kindOf nested
    assertBool
      (Text.unpack kind <> " is not a described " <> Text.unpack name)
      (kind `elem` map variantKind variants)
 where
  nestedSums =
    [ variants
    | Variant _ _ fields <- descriptionAnswers (toolDescription tool)
    , Field n (ShapeSum _ variants) _ _ <- fields
    , n == name
    ]

kindOf :: Value -> IO Text
kindOf value = do
  kind <- fieldOf "kind" value
  case kind of
    String text -> pure text
    other -> assertFailure $ "kind is not a string: " <> show other

fieldOf :: Text -> Value -> IO Value
fieldOf name (Object o)
  | Just value <- KeyMap.lookup (Key.fromText name) o = pure value
fieldOf name value =
  assertFailure $ "no " <> Text.unpack name <> " in " <> show value
