{-# LANGUAGE OverloadedStrings #-}

{- | The daemon's request loop, run in-process on a temporary socket against
the warm fixture and driven by the library's own client functions. Everything
that needs a second process (spawn, the locks, the readiness pipe, replace,
idle exit observed from outside) is hand smoke, per the plan.
-}
module Test.Interact.Daemon.Server (tests) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.TypeChecking.Monad (TCState)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (
  Async,
  poll,
  race,
  wait,
  waitCatch,
  withAsync,
 )
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (doesFileExist)
import System.FileLock (SharedExclusive (..), tryLockFile, unlockFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import AgdaMCP.Interact.CLI (defaultIdleTimeout)
import AgdaMCP.Interact.Daemon.Client (control, hello, openConnection, request)
import AgdaMCP.Interact.Daemon.Key (Key (..), Paths (..))
import AgdaMCP.Interact.Daemon.Protocol (
  BuildIdentity (..),
  Control (..),
  HelloResponse (..),
  Info (..),
  Request (..),
  Response (..),
  receive,
  send,
 )
import AgdaMCP.Interact.Daemon.Server (Daemon (..), serve)
import AgdaMCP.Options (AgdaFlags (..), defaultFlags)
import AgdaMCP.Tools.Goal qualified as Goal
import AgdaMCP.Tools.State (ToolState, runToolM)
import Test.Tool.Harness (loadId, withFixtureToolState)

tests :: IO TCState -> TestTree
tests warm =
  testGroup
    "Server"
    [ testCase "the info file is written before readiness is signalled" $
        withServer warm defaultIdleTimeout $ \server -> do
          decoded <- Aeson.eitherDecodeFileStrict (infoPath $ serverPaths server)
          infoRoot <$> decoded @?= Right (keyRoot $ serverKey server)
          infoFlags <$> decoded @?= Right ["--library-file", librariesFile server]
          infoBuild <$> decoded @?= Right thisBuild
    , testCase "load then goal through the socket answer as the tool does directly" $
        withServer warm defaultIdleTimeout $ \server -> do
          loaded <- call server (Request "load" (loadArguments server))
          responseOk loaded @?= True
          assertContains "Load ID: L1-k7x2" (responseText loaded)
          direct <-
            runToolM (serverState server) $
              Goal.goal
                Goal.Request
                  { Goal.goalRequestLoadId = loadId 1
                  , Goal.goalRequestGoalId = 0
                  , Goal.goalRequestNormalization = Simplified
                  }
          answered <- call server (Request "goal" goalArguments)
          answered
            @?= Response
              True
              (either id id $ Goal.renderResponse direct)
              (Aeson.toJSON direct)
    , testCase "an unknown tool is refused, not an error" $
        withServer warm defaultIdleTimeout $ \server -> do
          answered <- call server (Request "nope" Map.empty)
          answered
            @?= Response
              False
              "unknown tool \"nope\""
              ( Aeson.object
                  [ "kind" Aeson..= Aeson.String "unknown_tool"
                  , "tool" Aeson..= Aeson.String "nope"
                  , "message" Aeson..= Aeson.String "unknown tool \"nope\""
                  ]
              )
    , testCase "a mismatched hello is answered with the daemon's build, then closed" $
        withServer warm defaultIdleTimeout $ \server ->
          withConnection server $ \connection -> do
            reply <- hello connection thisBuild {buildSize = 1}
            reply @?= Just (BuildMismatch thisBuild)
            after <- receive connection :: IO (Maybe Response)
            after @?= Nothing
    , testCase "status before any load says nothing is loaded" $
        withServer warm defaultIdleTimeout $ \server -> do
          answered <- callControl server ControlStatus
          answered @?= nothingLoaded
    , testCase "status after a load names the file and the load id" $
        withServer warm defaultIdleTimeout $ \server -> do
          _ <- call server (Request "load" (loadArguments server))
          answered <- callControl server ControlStatus
          answered @?= loadedStatus server
    , testCase "stop finishes the server and cleans up its files" $
        withServer warm defaultIdleTimeout $ \server -> do
          answered <- callControl server ControlStop
          answered
            @?= Response
              True
              "Stopping"
              (Aeson.object ["kind" Aeson..= Aeson.String "stopping"])
          finished <- timeout (seconds 10) (wait $ serverThread server)
          finished @?= Just ()
          socketLeft <- doesFileExist (socketPath $ serverPaths server)
          socketLeft @?= False
          infoLeft <- doesFileExist (infoPath $ serverPaths server)
          infoLeft @?= False
          lock <- tryLockFile (daemonLockPath $ serverPaths server) Exclusive
          case lock of
            Nothing -> assertFailure "the daemon lock is still held"
            Just held -> unlockFile held
    , testCase "a client that vanishes mid-request does not take the server with it" $
        withServer warm defaultIdleTimeout $ \server -> do
          withConnection server $ \connection -> do
            reply <- hello connection thisBuild
            reply @?= Just BuildOk
            send connection (Request "load" (loadArguments server))
          -- The reply is discarded, but the load stands.
          settled <- timeout (seconds 60) $ awaitStatus server (loadedStatus server)
          settled @?= Just ()
          alive <- poll (serverThread server)
          assertBool "the server exited" (maybe True (const False) alive)
    , testCase "an idle server exits by itself" $
        withServer warm 1 $ \server -> do
          finished <- timeout (seconds 10) (wait $ serverThread server)
          finished @?= Just ()
    ]

-- Harness

data Server = Server
  { serverKey :: Key
  , serverPaths :: Paths
  , serverState :: MVar ToolState
  , serverThread :: Async ()
  , fixturePath :: FilePath
  , librariesFile :: FilePath
  }

{- | Serve the warm fixture on a socket under a temporary directory, hand the
server to the action once readiness has been signalled, and cancel it on the
way out.
-}
withServer :: IO TCState -> Int -> (Server -> IO a) -> IO a
withServer warm idleTimeout k =
  withFixtureToolState warm ["test/fixtures/HoleNatural.agda"] $ \paths toolState ->
    withSystemTempDirectory "agda-mcp-daemon" $ \directory -> do
      path <- case paths of
        [path] -> pure path
        _ -> assertFailure "withFixtureToolState answered a different length"
      let libraries = takeDirectory path </> "libraries"
          key =
            Key
              { keyRoot = takeDirectory path
              , keyFlags = defaultFlags {libraryFile = Just libraries}
              }
          daemonPaths =
            Paths
              { socketPath = directory </> "socket"
              , infoPath = directory </> "info"
              , logPath = directory </> "log"
              , spawnLockPath = directory </> "spawn.lock"
              , daemonLockPath = directory </> "daemon.lock"
              }
      stateVariable <- newMVar toolState
      ready <- newEmptyMVar
      let daemon =
            Daemon
              { daemonKey = key
              , daemonPaths = daemonPaths
              , daemonBuild = thisBuild
              , daemonIdleTimeout = idleTimeout
              , daemonReady = putMVar ready ()
              }
      withAsync (serve daemon stateVariable) $ \thread -> do
        started <- race (waitCatch thread) (takeMVar ready)
        case started of
          Left outcome ->
            assertFailure $ "the server exited before readiness: " <> show outcome
          Right () -> pure ()
        k
          Server
            { serverKey = key
            , serverPaths = daemonPaths
            , serverState = stateVariable
            , serverThread = thread
            , fixturePath = path
            , librariesFile = libraries
            }

-- | The build the test server runs as; any other build is a mismatch.
thisBuild :: BuildIdentity
thisBuild =
  BuildIdentity
    { buildExecutable = "/test/agda-interact"
    , buildSize = 100
    , buildModified = 1
    }

withConnection :: Server -> (Handle -> IO a) -> IO a
withConnection server = bracket (openConnection $ serverPaths server) hClose

-- | One request over one connection, as a client of the same build.
call :: Server -> Request -> IO Response
call server body =
  withConnection server $ \connection -> do
    reply <- hello connection thisBuild
    reply @?= Just BuildOk
    request connection body

callControl :: Server -> Control -> IO Response
callControl server command =
  withConnection server $ \connection -> control connection command

-- | Poll status until it answers as expected.
awaitStatus :: Server -> Response -> IO ()
awaitStatus server expected = do
  answered <- callControl server ControlStatus
  if answered == expected
    then pure ()
    else threadDelay 100000 >> awaitStatus server expected

nothingLoaded :: Response
nothingLoaded =
  Response
    True
    "Nothing loaded"
    ( Aeson.object
        [ "kind" Aeson..= Aeson.String "status"
        , "load" Aeson..= Aeson.Null
        ]
    )

loadedStatus :: Server -> Response
loadedStatus server =
  Response
    True
    ("Loaded: " <> Text.pack (fixturePath server) <> " (L1-k7x2)")
    ( Aeson.object
        [ "kind" Aeson..= Aeson.String "status"
        , "load"
            Aeson..= Aeson.object
              [ "load_id" Aeson..= Aeson.String "L1-k7x2"
              , "path" Aeson..= fixturePath server
              ]
        ]
    )

loadArguments :: Server -> Map Text Aeson.Value
loadArguments server =
  Map.fromList [("path", Aeson.String $ Text.pack $ fixturePath server)]

goalArguments :: Map Text Aeson.Value
goalArguments =
  Map.fromList [("load_id", Aeson.String "L1-k7x2"), ("goal", Aeson.Number 0)]

seconds :: Int -> Int
seconds = (* 1000000)

assertContains :: Text -> Text -> IO ()
assertContains needle haystack =
  assertBool
    (show needle <> " not found in " <> show haystack)
    (needle `Text.isInfixOf` haystack)
