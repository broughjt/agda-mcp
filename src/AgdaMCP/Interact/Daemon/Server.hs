{-# LANGUAGE OverloadedStrings #-}

{- | The daemon. An Agda session behind a Unix socket which dispatches requests
over the same tool table the MCP server uses.
-}
module AgdaMCP.Interact.Daemon.Server (
  Daemon (..),
  serve,
) where

import Agda.Version (version)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (cancel, link, withAsync)
import Control.Concurrent.MVar (
  MVar,
  newEmptyMVar,
  readMVar,
  takeMVar,
  tryPutMVar,
 )
import Control.Concurrent.STM (
  TVar,
  atomically,
  check,
  modifyTVar',
  newTVarIO,
  readTVar,
  writeTVar,
 )
import Control.Exception (IOException, catch, finally, try)
import Control.Monad (forever, void)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import GHC.Clock (getMonotonicTime)
import Network.Socket (
  Family (AF_UNIX),
  SockAddr (SockAddrUnix),
  Socket,
  SocketType (Stream),
  accept,
  bind,
  close,
  defaultProtocol,
  listen,
  socket,
  socketToHandle,
 )
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FileLock (SharedExclusive (Exclusive), tryLockFile, unlockFile)
import System.IO (
  Handle,
  IOMode (ReadWriteMode),
  hClose,
  hPutStrLn,
  stderr,
 )
import System.Posix.Process (getProcessID)

import AgdaMCP.Interact.Daemon.Key (Key (..), Paths (..))
import AgdaMCP.Interact.Daemon.Protocol (
  BuildIdentity,
  Control (..),
  HelloRequest (..),
  HelloResponse (..),
  Info (..),
  MalformedLine,
  Request (..),
  Response (..),
  receive,
  send,
 )
import AgdaMCP.Options (renderFlags)
import AgdaMCP.Tools (allTools)
import AgdaMCP.Tools.Description (Description (..))
import AgdaMCP.Tools.Json (kinded)
import AgdaMCP.Tools.LoadId (
  CurrentLoad (..),
  LoadGeneration (..),
  currentLoadId,
  renderLoadId,
 )
import AgdaMCP.Tools.State (ToolState (..))
import AgdaMCP.Tools.Tool (Answer (..), Tool (..))

data Daemon = Daemon
  { daemonKey :: Key
  , daemonPaths :: Paths
  , daemonBuild :: BuildIdentity
  , daemonIdleTimeout :: Int
  -- ^ Seconds without a request after which the daemon exits.
  , daemonReady :: IO ()
  {- ^ Run once the socket is listening and the info file is written. The
  executable prints @ready@ to the pipe its parent is waiting on.
  -}
  }

{- | Serve until told to stop or left idle. In order: take the daemon lock
(exit if another daemon holds it), unlink a stale socket, bind and listen,
write the info file, signal readiness, then loop. Each connection runs in its
own thread. Shutdown closes the listener and unlinks the socket first, then
waits for in-flight connections, then unlinks the info file.
-}
serve :: Daemon -> MVar ToolState -> IO ()
serve daemon stateVariable = do
  held <- tryLockFile (daemonLockPath paths) Exclusive
  case held of
    Nothing -> do
      hPutStrLn stderr $
        "another agda-interact daemon already serves "
          <> keyRoot (daemonKey daemon)
      exitWith $ ExitFailure 2
    Just lock -> (`finally` unlockFile lock) $ do
      removeIfPresent $ socketPath paths
      listener <- socket AF_UNIX Stream defaultProtocol
      bind listener $ SockAddrUnix $ socketPath paths
      listen listener 16
      writeInfo
      stopRequested <- newEmptyMVar
      inFlight <- newTVarIO 0
      lastActivity <- newTVarIO =<< getMonotonicTime
      let session =
            Session
              { daemon = daemon
              , stateVariable = stateVariable
              , stopRequested = stopRequested
              , inFlight = inFlight
              , lastActivity = lastActivity
              }
      withAsync (acceptLoop session listener) $ \acceptor ->
        withThread (idleWatch session) $ do
          link acceptor
          daemonReady daemon
          takeMVar stopRequested
          -- Stop taking connections before the in-flight ones drain, so a
          -- late client finds no socket rather than an unanswered one. The
          -- acceptor goes first: closing the listener under it would end it
          -- with an error, and it is linked.
          cancel acceptor
          close listener
          removeIfPresent $ socketPath paths
          atomically $ readTVar inFlight >>= check . (== 0)
          removeIfPresent $ infoPath paths
 where
  paths = daemonPaths daemon

  writeInfo = do
    pid <- getProcessID
    started <- getCurrentTime
    ByteString.Lazy.writeFile (infoPath paths) $
      encode
        Info
          { infoRoot = keyRoot $ daemonKey daemon
          , infoFlags = renderFlags $ keyFlags $ daemonKey daemon
          , infoPid = fromIntegral pid
          , infoBuild = daemonBuild daemon
          , infoAgdaVersion = Text.pack version
          , infoStarted = started
          }

-- | What the connection handlers and the idle watcher share.
data Session = Session
  { daemon :: Daemon
  , stateVariable :: MVar ToolState
  , stopRequested :: MVar ()
  , inFlight :: TVar Int
  , lastActivity :: TVar Double
  -- ^ Monotonic seconds at the end of the latest connection.
  }

{- | Run the body with a thread alongside it, killed when the body finishes.
Linked, so the thread dying takes the daemon down rather than leaving it
deaf.
-}
withThread :: IO () -> IO a -> IO a
withThread action body = withAsync action $ \thread -> link thread >> body

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
  present <- doesFileExist path
  if present then removeFile path else pure ()

{- | Accept connections until killed. The in-flight count is raised here rather
than in the connection's thread, so shutdown never sees a connection that has
been accepted but not yet counted.
-}
acceptLoop :: Session -> Socket -> IO ()
acceptLoop session listener = forever $ do
  (connection, _) <- accept listener
  atomically $ modifyTVar' (inFlight session) (+ 1)
  void $ forkIO $ serveConnection session connection `finally` finish
 where
  finish = do
    now <- getMonotonicTime
    atomically $ do
      modifyTVar' (inFlight session) (subtract 1)
      writeTVar (lastActivity session) now

{- | One connection: the hello or control line, then for an accepted hello
the request. The tool runs to completion whatever the client does; a write
that fails because the client has gone is dropped, and the state change
stands.
-}
serveConnection :: Session -> Socket -> IO ()
serveConnection session connection = do
  handle_ <- socketToHandle connection ReadWriteMode
  (exchange handle_ `catch` clientGone `catch` malformed)
    `finally` void (try (hClose handle_) :: IO (Either IOException ()))
 where
  exchange :: Handle -> IO ()
  exchange handle_ = do
    hello <- receive handle_
    case hello of
      Nothing -> pure ()
      Just (HelloControl ControlStatus) -> send handle_ =<< status session
      Just (HelloControl ControlStop) -> do
        send handle_ stopping
        void $ tryPutMVar (stopRequested session) ()
      Just (HelloBuild build)
        | build == daemonBuild (daemon session) -> do
            send handle_ BuildOk
            request <- receive handle_
            maybe (pure ()) (\r -> send handle_ =<< dispatch session r) request
        | otherwise -> send handle_ $ BuildMismatch $ daemonBuild $ daemon session

  clientGone :: IOException -> IO ()
  clientGone _ = pure ()

  malformed :: MalformedLine -> IO ()
  malformed _ = pure ()

dispatch :: Session -> Request -> IO Response
dispatch session (Request name arguments) =
  case Map.lookup name byName of
    Nothing ->
      pure $
        Response False message $
          kinded "unknown_tool" ["tool" .= name, "message" .= message]
     where
      message = "unknown tool " <> Text.pack (show name)
    Just tool -> do
      answer <- toolCall tool (stateVariable session) arguments
      pure
        Response
          { responseOk = answerOk answer
          , responseText = answerText answer
          , responseValue = answerValue answer
          }

byName :: Map Text Tool
byName =
  Map.fromList
    [(descriptionName $ toolDescription tool, tool) | tool <- allTools]

-- | The current load, read under the state's lock so a running call answers first.
status :: Session -> IO Response
status session = do
  generation <- toolLoadGeneration <$> readMVar (stateVariable session)
  pure $ case (currentLoadId generation, currentLoad generation) of
    (Just loadId, Just load) ->
      let path = Text.pack $ currentLoadPath load
          rendered = renderLoadId loadId
       in Response
            True
            ("Loaded: " <> path <> " (" <> rendered <> ")")
            ( kinded
                "status"
                ["load" .= object ["load_id" .= rendered, "path" .= path]]
            )
    _ -> Response True "Nothing loaded" $ kinded "status" ["load" .= Null]

stopping :: Response
stopping = Response True "Stopping" $ kinded "stopping" []

{- | Fire the shutdown once no connection has been open for the idle timeout.
The clock is read again after every sleep, so a connection that arrives while
sleeping pushes the deadline out.
-}
idleWatch :: Session -> IO ()
idleWatch session = forever $ do
  now <- getMonotonicTime
  (open, latest) <-
    atomically $
      (,) <$> readTVar (inFlight session) <*> readTVar (lastActivity session)
  let deadline = latest + fromIntegral (daemonIdleTimeout $ daemon session)
  if open == 0 && now >= deadline
    then do
      void $ tryPutMVar (stopRequested session) ()
      threadDelay maxBound
    else threadDelay $ max 100000 $ ceiling $ (deadline - now) * 1000000
