{-# LANGUAGE OverloadedStrings #-}

module Give (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as ByteString
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Directory (removeFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
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
import AgdaMCP.Tools.Load (
  ContextEntry (..),
  ContextEntryAttributes (..),
  Goal (..),
  GoalShape (..),
  LoadRequest (..),
  load,
 )

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
    [ testCase "single give uses code-point offsets with non-ASCII source" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          -- The arrows before the hole are multi-byte in UTF-8. The exact-byte
          -- assertion below is therefore a check against an accidental use of
          -- byte offsets in place of Agda's code-point offsets.
          assertBool "fixture has multi-byte source before the hole" $
            ByteString.length original > Text.length (decodeUtf8 original)
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
    , testCase "BOM and CRLF source is normalized and spliced correctly" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          let bom = ByteString.pack [0xEF, 0xBB, 0xBF]
              crlfSource =
                bom <> encodeUtf8 (Text.replace "\n" "\r\n" (decodeUtf8 original))
          ByteString.writeFile path crlfSource
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "y")])
          case giveOutcome response of
            GiveApplied [_] -> pure ()
            other -> assertFailure ("expected one applied edit, got " <> show other)
          (goals, _, _, _) <- expectLoaded (giveReload response)
          map goalId goals @?= []
          after <- ByteString.readFile path
          -- Give writes Agda's normalized view: UTF-8 without a BOM and with
          -- LF endings.
          after @?= withHoleGiven "y" original
    , testCase "written text is Agda's elaboration of the submitted text" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "(y)")])
          edit <- case giveOutcome response of
            GiveApplied [e] -> pure e
            other -> assertFailure ("expected one applied edit, got " <> show other)
          editSubmitted edit @?= "(y)"
          editText edit @?= "y"
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
    , testCase "give preserves prose around a literate Markdown module" $
        withFixture "Literate.lagda.md" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "x")])
          case giveOutcome response of
            GiveApplied [_] -> pure ()
            other -> assertFailure ("expected one applied edit, got " <> show other)
          (goals, _, _, _) <- expectLoaded (giveReload response)
          map goalId goals @?= []
          after <- ByteString.readFile path
          after @?= withHoleGiven "x" original
    , testCase "give preserves a missing trailing newline" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          let source = decodeUtf8 original
          assertBool "fixture ends in a newline" ("\n" `Text.isSuffixOf` source)
          let eofSource = Text.dropEnd 1 source
          ByteString.writeFile path (encodeUtf8 eofSource)
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "y")])
          case giveOutcome response of
            GiveApplied [_] -> pure ()
            other -> assertFailure ("expected one applied edit, got " <> show other)
          (goals, _, _, _) <- expectLoaded (giveReload response)
          map goalId goals @?= []
          after <- decodeUtf8 <$> ByteString.readFile path
          after @?= Text.replace "{!!}" "y" eofSource
          assertBool "give did not append a newline" (not ("\n" `Text.isSuffixOf` after))
    , testCase "question-mark hole is spliced over its exact span" $
        withFixture "Hole.agda" $ \path -> do
          original <- decodeUtf8 <$> ByteString.readFile path
          let questionSource = Text.replace "{!!}" "?" original
          ByteString.writeFile path (encodeUtf8 questionSource)
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "y")])
          case giveOutcome response of
            GiveApplied [edit] ->
              spanCoordinates (editSpan edit) @?= ((8, 12), (8, 13))
            other -> assertFailure ("expected one applied edit, got " <> show other)
          (goals, _, _, _) <- expectLoaded (giveReload response)
          map goalId goals @?= []
          after <- decodeUtf8 <$> ByteString.readFile path
          after @?= Text.replace "?" "y" questionSource
    , testCase "content hole is spliced over its exact span" $
        withFixture "Hole.agda" $ \path -> do
          original <- decodeUtf8 <$> ByteString.readFile path
          let contentSource = Text.replace "{!!}" "{! y !}" original
          ByteString.writeFile path (encodeUtf8 contentSource)
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "y")])
          case giveOutcome response of
            GiveApplied [edit] ->
              spanCoordinates (editSpan edit) @?= ((8, 12), (8, 19))
            other -> assertFailure ("expected one applied edit, got " <> show other)
          (goals, _, _, _) <- expectLoaded (giveReload response)
          map goalId goals @?= []
          after <- decodeUtf8 <$> ByteString.readFile path
          after @?= Text.replace "{! y !}" "y" contentSource
    , testCase "partial give renumbers the remaining goals" $
        withFixture "TwoHoles.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "zero")])
          case giveOutcome response of
            GiveApplied [edit] -> editGoal edit @?= InteractionId 0
            other -> assertFailure ("expected one applied edit, got " <> show other)
          -- The remaining hole, formerly ?1, is renumbered to ?0 by the resync.
          (goals, _, _, _) <- expectLoaded (giveReload response)
          case goals of
            [Goal goal s _ _] -> do
              goal @?= InteractionId 0
              spanCoordinates s @?= ((11, 7), (11, 11))
            other -> assertFailure ("expected one goal, got " <> show other)
          after <- ByteString.readFile path
          after
            @?= encodeUtf8
              (Text.replace "one = {!!}" "one = zero" (decodeUtf8 original))
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
    , testCase "out-of-scope expression is rejected" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path)
                *> give (GiveRequest path [(InteractionId 0, "nonexistent")])
          rejection <- expectRejected (giveOutcome response)
          rejectedGoal rejection @?= InteractionId 0
          agdaErrorSpan (rejectedError rejection) @?= Nothing
          assertBool "mentions scope" $
            "in scope" `Text.isInfixOf` agdaErrorMessage (rejectedError rejection)
          after <- ByteString.readFile path
          after @?= original
          (goals, _, _, _) <- expectLoaded (giveReload response)
          length goals @?= 1
    , testCase "unparseable expression is rejected" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path) *> give (GiveRequest path [(InteractionId 0, "y (")])
          rejection <- expectRejected (giveOutcome response)
          rejectedGoal rejection @?= InteractionId 0
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
            [Goal _ s _ _] -> spanCoordinates s @?= ((9, 12), (9, 16))
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
            [Goal _ _ (GoalOfType ty) _] -> ty @?= "Set"
            other -> assertFailure ("expected one typed goal, got " <> show other)
    , testCase "give refused for a changed file applies when retried" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          let edited = "-- pushed down\n" <> original
          let request = GiveRequest path [(InteractionId 0, "y")]
          (first, second) <- runSession $ do
            _ <- load (LoadRequest path)
            liftIO (ByteString.writeFile path edited)
            (,) <$> give request <*> give request
          case giveOutcome first of
            GiveFileChanged -> pure ()
            other -> assertFailure ("expected GiveFileChanged, got " <> show other)
          -- The refusal's resync loaded the edited file, so the retry applies
          -- (at the shifted span).
          case giveOutcome second of
            GiveApplied [edit] -> spanCoordinates (editSpan edit) @?= ((9, 12), (9, 16))
            other -> assertFailure ("expected one applied edit, got " <> show other)
          (goals, _, _, _) <- expectLoaded (giveReload second)
          map goalId goals @?= []
          after <- ByteString.readFile path
          after @?= withHoleGiven "y" edited
    , testCase "deleting the file between load and give is an IO error" $
        withFixture "Hole.agda" $ \path -> do
          response <- runSession $ do
            _ <- load (LoadRequest path)
            liftIO (removeFile path)
            give (GiveRequest path [(InteractionId 0, "y")])
          case giveOutcome response of
            GiveIOError message ->
              assertBool "names the failure" ("does not exist" `Text.isInfixOf` message)
            other -> assertFailure ("expected GiveIOError, got " <> show other)
          _ <- expectLoadFailed (giveReload response)
          pure ()
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
    , testCase "give to a previously loaded but displaced file is refused" $
        withFixture "Hole.agda" $ \holePath ->
          withFixture "TwoHoles.agda" $ \twoPath -> do
            original <- ByteString.readFile holePath
            response <- runSession $ do
              _ <- load (LoadRequest holePath)
              -- Loading another file destroys the first file's interaction
              -- points, so its previously valid goal IDs must be refused.
              _ <- load (LoadRequest twoPath)
              give (GiveRequest holePath [(InteractionId 0, "y")])
            case giveOutcome response of
              GiveNotLoaded -> pure ()
              other -> assertFailure ("expected GiveNotLoaded, got " <> show other)
            (goals, _, _, _) <- expectLoaded (giveReload response)
            length goals @?= 1
            after <- ByteString.readFile holePath
            after @?= original
    , testCase "equivalent non-canonical path is recognized as loaded" $
        withFixture "Hole.agda" $ \path -> do
          original <- ByteString.readFile path
          let directory = takeDirectory path
              nonCanonicalPath =
                directory
                  </> ".."
                  </> takeFileName directory
                  </> takeFileName path
          assertBool "test path contains a parent-directory segment" $
            nonCanonicalPath /= path
          response <-
            runSession $
              load (LoadRequest path)
                *> give
                  (GiveRequest nonCanonicalPath [(InteractionId 0, "y")])
          case giveOutcome response of
            GiveApplied [_] -> pure ()
            other -> assertFailure ("expected one applied edit, got " <> show other)
          (goals, _, _, _) <- expectLoaded (giveReload response)
          map goalId goals @?= []
          after <- ByteString.readFile path
          after @?= withHoleGiven "y" original
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
    , testCase "bogus goal id mid-batch counts discarded and skipped gives" $
        withFixture "ThreeHoles.agda" $ \path -> do
          original <- ByteString.readFile path
          response <-
            runSession $
              load (LoadRequest path)
                *> give
                  ( GiveRequest
                      path
                      [ (InteractionId 0, "zero")
                      , (InteractionId 9, "zero")
                      , (InteractionId 2, "zero")
                      ]
                  )
          case giveOutcome response of
            GiveUnknownGoal goal batch -> do
              goal @?= InteractionId 9
              batchDiscarded batch @?= 1
              batchSkipped batch @?= 1
            other -> assertFailure ("expected GiveUnknownGoal, got " <> show other)
          after <- ByteString.readFile path
          after @?= original
          (goals, _, _, _) <- expectLoaded (giveReload response)
          length goals @?= 3
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
    , testCase "resync report carries the remaining goals' contexts" $
        -- Contexts re-anchor the agent across the id renumbering the give
        -- causes. The surviving goals renumber to ?0 and ?1, and each line
        -- still shows what is in scope there.
        withFixture "Context.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> give (GiveRequest path [(InteractionId 0, "y")])
          case giveOutcome response of
            GiveApplied [_] -> pure ()
            other -> assertFailure ("expected one applied edit, got " <> show other)
          (goals, _, _, _) <- expectLoaded (giveReload response)
          map goalId goals @?= [InteractionId 0, InteractionId 1]
          map goalContext goals
            @?= [
                  [ ContextEntry "x" True "x" "Nat" Nothing True noAttributes
                  , ContextEntry "y" True "y" "Nat" Nothing True noAttributes
                  ]
                ,
                  [ ContextEntry
                      "one"
                      True
                      "one"
                      "Nat"
                      (Just "suc zero")
                      True
                      noAttributes
                  ]
                ]
    ]

noAttributes :: ContextEntryAttributes
noAttributes = ContextEntryAttributes Nothing False Nothing Nothing False
