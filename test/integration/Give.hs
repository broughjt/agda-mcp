{-# LANGUAGE OverloadedStrings #-}

module Give (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as ByteString
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Agda.Syntax.Common (InteractionId (InteractionId))

import AgdaMCP.Tools.Common (AgdaError (..))
import AgdaMCP.Tools.Give (
  BatchPosition (..),
  Edit (..),
  GiveOutcome (..),
  GiveRejection (..),
  GiveRequest (..),
  GiveResponse (..),
  give,
 )
import AgdaMCP.Tools.Load (Goal (..), GoalShape (..), LoadRequest (..), load)

import Common (
  expectLoadFailed,
  expectLoaded,
  expectRejected,
  runSession,
  spanCoordinates,
  withFixture,
  withHoleGiven,
 )

tests :: TestTree
tests =
  testGroup
    "give"
    [ testCase "single give splices the hole and resyncs to zero goals" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "y")])
          edit <- case giveOutcome response of
            GiveApplied [e] -> pure e
            other -> assertFailure ("expected one applied edit, got " <> show other)
          editGoal edit @?= InteractionId 0
          editSubmitted edit @?= "y"
          editText edit @?= "y"
          spanCoordinates (editSpan edit) @?= ((8, 12), (8, 16))
          (goals, _, _, _) <- expectLoaded (giveReload response)
          map goalId goals @?= []
          after <- ByteString.readFile path
          after @?= withHoleGiven "y" original
    , testCase "two-hole batch splices both in one call" $
        withFixture "TwoHoles.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path)
                *> give (GiveRequest path [(InteractionId 0, "zero"), (InteractionId 1, "zero")])
          case giveOutcome response of
            GiveApplied edits -> map editGoal edits @?= [InteractionId 0, InteractionId 1]
            other -> assertFailure ("expected two applied edits, got " <> show other)
          (goals, _, _, _) <- expectLoaded (giveReload response)
          map goalId goals @?= []
          after <- ByteString.readFile path
          after @?= withHoleGiven "zero" original
    , testCase "a later rejection discards earlier gives" $
        withFixture "TwoHoles.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path)
                *> give (GiveRequest path [(InteractionId 0, "zero"), (InteractionId 1, "suc")])
          rejection <- expectRejected (giveOutcome response)
          rejectedGoal rejection @?= InteractionId 1
          batchDiscarded (rejectedBatch rejection) @?= 1
          batchSkipped (rejectedBatch rejection) @?= 0
          after <- ByteString.readFile path
          after @?= original
          (goals, _, _, _) <- expectLoaded (giveReload response)
          length goals @?= 2
    , testCase "a middle rejection counts discarded and skipped gives" $
        withFixture "ThreeHoles.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path)
                *> give
                  ( GiveRequest
                      path
                      [ (InteractionId 0, "zero")
                      , (InteractionId 1, "suc")
                      , (InteractionId 2, "zero")
                      ]
                  )
          rejection <- expectRejected (giveOutcome response)
          rejectedGoal rejection @?= InteractionId 1
          batchDiscarded (rejectedBatch rejection) @?= 1
          batchSkipped (rejectedBatch rejection) @?= 1
          after <- ByteString.readFile path
          after @?= original
          (goals, _, _, _) <- expectLoaded (giveReload response)
          length goals @?= 3
    , testCase "ill-typed single give leaves the file untouched" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "suc")])
          rejection <- expectRejected (giveOutcome response)
          rejectedGoal rejection @?= InteractionId 0
          agdaErrorSpan (rejectedError rejection) @?= Nothing
          assertBool "the hole's span is reported" (isJust (rejectedSpan rejection))
          batchDiscarded (rejectedBatch rejection) @?= 0
          batchSkipped (rejectedBatch rejection) @?= 0
          after <- ByteString.readFile path
          after @?= original
          (goals, _, _, _) <- expectLoaded (giveReload response)
          length goals @?= 1
    , testCase "hole-moving disk edit between load and give is refused" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          let edited = "-- pushed down\n" <> original
          response <- runSession $ do
            _ <- load (LoadRequest path)
            liftIO $ ByteString.writeFile path edited
            give (GiveRequest path [(InteractionId 0, "y")])
          case giveOutcome response of
            GiveFileChanged -> pure ()
            other -> assertFailure ("expected GiveFileChanged, got " <> show other)
          after <- ByteString.readFile path
          after @?= edited
          -- The resync sees the hand-edited file
          (goals, _, _, _) <- expectLoaded (giveReload response)
          case goals of
            [Goal _ s _] -> spanCoordinates s @?= ((9, 12), (9, 16))
            other -> assertFailure ("expected one goal, got " <> show other)
    , testCase "span-preserving disk edit is refused via the fingerprint" $
        -- A same-width edit leaves the hole offsets valid, but we still catch
        -- it
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          let edited =
                encodeUtf8 $
                  Text.replace
                    "plus : Nat → Nat → Nat"
                    "plus : Nat → Nat → Set"
                    (decodeUtf8 original)
          response <- runSession $ do
            _ <- load $ LoadRequest path
            liftIO $ ByteString.writeFile path edited
            give $ GiveRequest path [(InteractionId 0, "y")]
          case giveOutcome response of
            GiveFileChanged -> pure ()
            other -> assertFailure ("expected GiveFileChanged, got " <> show other)
          after <- ByteString.readFile path
          after @?= edited
          (goals, _, _, _) <- expectLoaded $ giveReload response
          case goals of
            [Goal _ _ (GoalOfType ty)] -> ty @?= "Set"
            other -> assertFailure ("expected one typed goal, got " <> show other)
    , testCase "give before load is refused and supplies the load" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          response <- runSession (give (GiveRequest path [(InteractionId 0, "y")]))
          case giveOutcome response of
            GiveNotLoaded -> pure ()
            other -> assertFailure ("expected GiveNotLoaded, got " <> show other)
          (goals, _, _, _) <- expectLoaded (giveReload response)
          length goals @?= 1
          after <- ByteString.readFile path
          after @?= original
    , testCase "give before load on a broken file resyncs to the load failure" $
        withFixture "TypeError.agda" $ \path -> do
          original <- ByteString.readFile path
          response <- runSession (give (GiveRequest path [(InteractionId 0, "zero")]))
          case giveOutcome response of
            GiveNotLoaded -> pure ()
            other -> assertFailure $ "expected GiveNotLoaded, got " <> show other
          _ <- expectLoadFailed (giveReload response)
          after <- ByteString.readFile path
          after @?= original
    , testCase "refused give retried in the same session applies" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          let request = GiveRequest path [(InteractionId 0, "y")]
          (first, second) <- runSession ((,) <$> give request <*> give request)
          case giveOutcome first of
            GiveNotLoaded -> pure ()
            other -> assertFailure ("expected GiveNotLoaded, got " <> show other)
          -- The refusal's load made the file current, so the retry applies.
          case giveOutcome second of
            GiveApplied [_] -> pure ()
            other -> assertFailure ("expected one applied edit, got " <> show other)
          (goals, _, _, _) <- expectLoaded $ giveReload second
          map goalId goals @?= []
          after <- ByteString.readFile path
          after @?= withHoleGiven "y" original
    , testCase "bogus goal id is reported with a resync" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 9, "y")])
          case giveOutcome response of
            GiveUnknownGoal goal batch -> do
              goal @?= InteractionId 9
              batchDiscarded batch @?= 0
              batchSkipped batch @?= 0
            other -> assertFailure ("expected GiveUnknownGoal, got " <> show other)
          (goals, _, _, _) <- expectLoaded $ giveReload response
          length goals @?= 1
          after <- ByteString.readFile path
          after @?= original
    , testCase "give underscore succeeds and strands a hidden metavariable" $
        -- Pins the status quo for dogfood UX gap 4; update this case when
        -- gap 4 is addressed.
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "_")])
          case giveOutcome response of
            GiveApplied [_] -> pure ()
            other -> assertFailure ("expected one applied edit, got " <> show other)
          (goals, metas, _, _) <- expectLoaded $ giveReload response
          map goalId goals @?= []
          assertBool "stranded hidden metavariable reported" (not $ null metas)
          after <- ByteString.readFile path
          after @?= withHoleGiven "_" original
    ]
