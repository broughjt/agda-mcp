{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Interruption (tests) where

import Agda.Interaction.Options (defaultOptions)
import Agda.TypeChecking.Monad (TCState)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (
  AsyncCancelled (..),
  async,
  cancel,
  wait,
  waitCatch,
 )
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (fromException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Numeric.Natural (Natural)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadGeneration (..))
import AgdaMCP.Tools.MCP (
  Budget (..),
  inspectBudget,
  loadBudget,
  parseTimeout,
 )
import AgdaMCP.Tools.State (
  ToolM,
  ToolState (..),
  newToolState,
  runToolM,
  withBudget,
 )
import Test.Tool.Harness (withFixtureToolState)

tests :: IO TCState -> TestTree
tests warm =
  testGroup
    "Interruption"
    [ agdaTests warm
    , runToolMTests
    , parseTimeoutTests
    ]

-- Interrupting Agda itself

agdaTests :: IO TCState -> TestTree
agdaTests warm =
  testGroup
    "interrupting a load"
    [ testCase "canceling one unwinds it and leaves the session as it was" $
        withSlowCheck warm $ \path stateVariable -> do
          running <- async $ runToolM stateVariable (loadOf path)
          threadDelay (seconds 2)
          canceled <- timeout (seconds 10) (cancel running)
          canceled @?= Just ()
          outcome <- waitCatch running
          case outcome of
            Left thrown ->
              fromException thrown @?= Just AsyncCancelled
            Right response ->
              assertFailure $
                "expected the canceled load to unwind, got " <> show response
          issued stateVariable >>= (@?= 0)
    , testCase "one that outruns its budget is abandoned" $
        withSlowCheck warm $ \path stateVariable -> do
          response <-
            runToolM stateVariable $ withBudget (seconds 1) (loadOf path)
          response @?= Nothing
          issued stateVariable >>= (@?= 0)
    ]
 where
  loadOf :: FilePath -> ToolM Load.Response
  loadOf path = Load.load Load.Request {Load.loadRequestPath = path}

  withSlowCheck ::
    IO TCState -> (FilePath -> MVar ToolState -> IO a) -> IO a
  withSlowCheck warmed k =
    withFixtureToolState warmed ["test/fixtures/SlowCheck.agda"] $
      \paths state -> case paths of
        [path] -> newMVar state >>= k path
        _ -> assertFailure "expected one staged fixture"

-- runToolM and withBudget, with actions that stand in for a tool

runToolMTests :: TestTree
runToolMTests =
  testGroup
    "runToolM"
    [ testCase "an action that finishes commits its state" $ do
        stateVariable <- freshState
        runToolM stateVariable (withBudget (seconds 10) issue)
          >>= (@?= Just ())
        issued stateVariable >>= (@?= 1)
    , testCase "an action that outruns its budget rolls its state back" $ do
        stateVariable <- freshState
        result <-
          runToolM stateVariable $
            withBudget (milliseconds 100) (issue >> sleep (seconds 10))
        result @?= Nothing
        issued stateVariable >>= (@?= 0)
    , testCase "a canceled action rolls back without stranding the state" $ do
        stateVariable <- freshState
        running <-
          async $ runToolM stateVariable (issue >> sleep (seconds 600))
        threadDelay (milliseconds 100)
        cancel running
        outcome <- waitCatch running
        case outcome of
          Left thrown -> fromException thrown @?= Just AsyncCancelled
          Right result ->
            assertFailure $
              "expected the canceled action to unwind, got " <> show result
        followed <- timeout (seconds 10) (issued stateVariable)
        followed @?= Just 0
    , testCase "a queued call is not charged for the wait" $ do
        stateVariable <- freshState
        holding <- async $ runToolM stateVariable (sleep $ seconds 2)
        threadDelay (milliseconds 100)
        queued <-
          async $ runToolM stateVariable (withBudget (milliseconds 500) issue)
        wait holding >>= (@?= ())
        wait queued >>= (@?= Just ())
        issued stateVariable >>= (@?= 1)
    ]

-- Timeout parsing

parseTimeoutTests :: TestTree
parseTimeoutTests =
  testGroup
    "parseTimeout"
    [ testCase "a call without one gets its tool's default" $ do
        parse inspectBudget Map.empty @?= Right (budgetDefault inspectBudget)
        parse loadBudget Map.empty @?= Right (budgetDefault loadBudget)
    , testCase "a whole number of seconds is taken as given" $
        parse inspectBudget (withTimeout $ Aeson.Number 5) @?= Right 5
    , testCase "zero is refused" $
        parse inspectBudget (withTimeout $ Aeson.Number 0) @?= Left refusal
    , testCase "a negative timeout is refused" $
        parse inspectBudget (withTimeout $ Aeson.Number (-1)) @?= Left refusal
    , testCase "a fractional timeout is refused" $
        parse inspectBudget (withTimeout $ Aeson.Number 1.5) @?= Left refusal
    , testCase "a timeout that is not a number is refused" $
        parse inspectBudget (withTimeout $ Aeson.String "30")
          @?= Left refusal
    , testCase "a timeout too large to convert is clamped" $
        parse inspectBudget (withTimeout $ Aeson.Number 1e30)
          @?= Right (maxBound `div` 1000000)
    ]
 where
  parse :: Budget -> Map Text Aeson.Value -> Either Text Int
  parse = parseTimeout

  withTimeout :: Aeson.Value -> Map Text Aeson.Value
  withTimeout = Map.singleton "timeout"

  refusal :: Text
  refusal = "expected timeout to be a whole number of seconds greater than zero"

-- Helpers

freshState :: IO (MVar ToolState)
freshState = newToolState defaultOptions >>= newMVar

issued :: MVar ToolState -> IO Natural
issued stateVariable =
  runToolM stateVariable (gets $ loadsIssued . toolLoadGeneration)

issue :: ToolM ()
issue = modify $ \state ->
  state
    { toolLoadGeneration =
        let generation = toolLoadGeneration state
         in generation {loadsIssued = 1 + loadsIssued generation}
    }

sleep :: Int -> ToolM ()
sleep = liftIO . threadDelay

seconds :: Int -> Int
seconds = (* 1000000)

milliseconds :: Int -> Int
milliseconds = (* 1000)
