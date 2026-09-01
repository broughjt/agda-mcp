{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.LoadId (tests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AgdaMCP.Tools.LoadId (
  CurrentLoad (..),
  LoadGeneration (..),
  LoadId (..),
  LoadIdRefusal (..),
  SessionNonce (..),
  currentLoadId,
  renderLoadId,
  renderLoadIdRefusal,
  requireCurrentLoad,
 )
import Test.Tool.Harness (fixtureNonce, loadId)

tests :: TestTree
tests =
  testGroup
    "LoadId"
    [ wireFormTests
    , requireCurrentLoadTests
    , renderRefusalTests
    ]

wireFormTests :: TestTree
wireFormTests =
  testGroup
    "wire form"
    [ testCase "renders as the counter then the session nonce" $
        renderLoadId (loadId 3) @?= "L3-k7x2"
    , testCase "parsing inverts rendering" $
        parse (renderLoadId (loadId 3)) @?= Right (loadId 3)
    , testCase "a nonce from another session parses to that session" $
        parse "L3-abcd" @?= Right (LoadId (SessionNonce "abcd") 3)
    , testCase "a bare counter is refused" $
        parse "L3" @?= Left (refusal "L3")
    , testCase "a counter with an empty nonce is refused" $
        parse "L3-" @?= Left (refusal "L3-")
    , testCase "an upper-case nonce is refused" $
        parse "L3-K7X2" @?= Left (refusal "L3-K7X2")
    , testCase "a missing L prefix is refused" $
        parse "3-k7x2" @?= Left (refusal "3-k7x2")
    , testCase "trailing whitespace is refused" $
        parse "L3-k7x2 " @?= Left (refusal "L3-k7x2 ")
    , testCase "a nonce of the wrong length is refused" $
        parse "L3-k7x" @?= Left (refusal "L3-k7x")
    ]
 where
  parse :: Text -> Either Text LoadId
  parse =
    first Text.pack
      . Aeson.parseEither Aeson.parseJSON
      . Aeson.String

  refusal :: Text -> Text
  refusal text =
    "Error in $: expected a load_id from the most recent result, such as \
    \\"L17-k7x2\", but got "
      <> Text.pack (show text)

requireCurrentLoadTests :: TestTree
requireCurrentLoadTests =
  testGroup
    "requireCurrentLoad"
    [ testCase "the current id is accepted" $
        requireCurrentLoad (generation 3) (loadId 3) @?= Right current
    , testCase "an older id from this session is stale" $
        requireCurrentLoad (generation 3) (loadId 2)
          @?= Left (StaleLoadId (loadId 3))
    , testCase "a newer id from this session is also stale" $
        requireCurrentLoad (generation 3) (loadId 4)
          @?= Left (StaleLoadId (loadId 3))
    , testCase "an id from another session is foreign, whatever its counter" $
        requireCurrentLoad (generation 3) (LoadId (SessionNonce "abcd") 3)
          @?= Left ForeignLoadId
    , testCase "nothing loaded refuses this session's id" $
        requireCurrentLoad (generation 3) {currentLoad = Nothing} (loadId 3)
          @?= Left NoCurrentLoad
    , testCase "nothing loaded refuses a foreign id the same way" $
        requireCurrentLoad
          (generation 3) {currentLoad = Nothing}
          (LoadId (SessionNonce "abcd") 3)
          @?= Left NoCurrentLoad
    , testCase "the current id carries the session nonce" $
        currentLoadId (generation 3) @?= Just (loadId 3)
    ]
 where
  generation :: Integer -> LoadGeneration
  generation issued =
    LoadGeneration
      { sessionNonce = fixtureNonce
      , loadsIssued = fromInteger issued
      , currentLoad = Just current
      }

  current :: CurrentLoad
  current =
    CurrentLoad
      { currentLoadPath = "/tmp/Example.agda"
      , currentLoadSourceHash = 0
      }

renderRefusalTests :: TestTree
renderRefusalTests =
  testGroup
    "renderLoadIdRefusal"
    [ testCase "a foreign id says the session is gone" $
        renderLoadIdRefusal ForeignLoadId
          @?= "The supplied load ID is from a session that no longer exists. \
              \Load the file again and use the load ID from that result."
    , testCase "a stale id names the current id in wire form" $
        renderLoadIdRefusal (StaleLoadId (loadId 2))
          @?= "The supplied load ID is from an earlier load. The current load \
              \ID is L2-k7x2. Each load makes fresh goal ID assignments, so use \
              \the most recently issued load ID and goal IDs. If you no longer \
              \have them, load the file again."
    ]
