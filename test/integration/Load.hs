{-# LANGUAGE OverloadedStrings #-}

module Load (tests) where

import Data.Maybe (isJust)
import Data.Text qualified as Text
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Agda.Syntax.Common (InteractionId (InteractionId))

import AgdaMCP.Tools.Common (AgdaError (..), NonFatalError (..), Warning (..))
import AgdaMCP.Tools.Load (
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  LoadRequest (..),
  load,
 )

import Common (
  expectLoadFailed,
  expectLoaded,
  runSession,
  spanCoordinates,
  withFixture,
 )

tests :: TestTree
tests =
  testGroup
    "load"
    [ testCase "file with one hole" $
        withFixture "Hole.agda" $ \path -> do
          response <- runSession $ load $ LoadRequest path
          (goals, metas, warnings, errors) <- expectLoaded response
          case goals of
            [Goal goalId' s (GoalOfType ty)] -> do
              goalId' @?= InteractionId 0
              ty @?= "Nat"
              spanCoordinates s @?= ((8, 12), (8, 16))
            other -> assertFailure ("expected one typed goal, got " <> show other)
          assertBool "no metas, warnings, or non-fatal errors" $
            null metas && null warnings && null errors
    , testCase "type error carries a file span" $
        withFixture "TypeError.agda" $ \path -> do
          response <- runSession (load (LoadRequest path))
          e <- expectLoadFailed response
          case agdaErrorSpan e of
            Just s -> spanCoordinates s @?= ((8, 7), (8, 10))
            Nothing -> assertFailure "expected an error span in the loaded file"
    , testCase "reload after an error yields an identical success" $
        withFixture "Hole.agda" $ \holePath ->
          withFixture "TypeError.agda" $ \badPath -> do
            (first, failed, second) <- runSession $ do
              first <- load $ LoadRequest holePath
              failed <- load $ LoadRequest badPath
              second <- load $ LoadRequest holePath
              pure (first, failed, second)
            _ <- expectLoadFailed failed
            (goals, _, _, _) <- expectLoaded first
            map goalId goals @?= [InteractionId 0]
            second @?= first
    , testCase "nonexistent file" $
        withSystemTempDirectory "agda-mcp-integration" $ \directory -> do
          let path = directory </> "Missing.agda"
          response <- runSession (load (LoadRequest path))
          e <- expectLoadFailed response
          agdaErrorMessage e
            @?= Text.pack ("Cannot read file " <> path <> ": does not exist.")
    , testCase "unreachable clause populates warnings" $
        withFixture "Unreachable.agda" $ \path -> do
          response <- runSession (load (LoadRequest path))
          (goals, _, warnings, _) <- expectLoaded response
          map goalId goals @?= []
          case warnings of
            [Warning (maybeSpan, _)] ->
              case maybeSpan of
                Just s -> spanCoordinates s @?= ((9, 1), (9, 14))
                Nothing -> assertFailure "expected the warning's span"
            other -> assertFailure ("expected one warning, got " <> show other)
    , testCase "safe postulate populates non-fatal errors" $
        withFixture "SafePostulate.agda" $ \path -> do
          response <- runSession (load (LoadRequest path))
          (goals, _, warnings, errors) <- expectLoaded response
          map goalId goals @?= []
          warnings @?= []
          case errors of
            [NonFatalError (Just s, message)] -> do
              spanCoordinates s @?= ((4, 11), (4, 25))
              assertBool "mentions SafeFlagPostulate" $
                "SafeFlagPostulate" `Text.isInfixOf` message
            other ->
              assertFailure ("expected one located non-fatal error, got " <> show other)
    , testCase "unsolved metas populate hidden metavariables" $
        withFixture "UnsolvedMetas.agda" $ \path -> do
          response <- runSession (load (LoadRequest path))
          (goals, metas, _, _) <- expectLoaded response
          map goalId goals @?= []
          assertBool "hidden metavariables reported" (not $ null metas)
          assertBool "at least one hidden metavariable has a span" $
            any (isJust . hiddenMetavariableSpan) metas
    ]
