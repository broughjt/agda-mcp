{-# LANGUAGE OverloadedStrings #-}

{- | The client side of the daemon: one connection per request, and the
connect protocol that starts or replaces a daemon on the way.
-}
module AgdaMCP.Interact.Daemon.Client (
  Plumbing (..),
  openConnection,
  hello,
  request,
  control,
  withDaemon,
  withConnection,
  withRunning,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (
  Exception,
  IOException,
  bracket,
  catch,
  displayException,
  finally,
  onException,
  throwIO,
  try,
 )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import GHC.Clock (getMonotonicTime)
import Network.Socket (
  Family (AF_UNIX),
  SockAddr (SockAddrUnix),
  SocketType (Stream),
  connect,
  defaultProtocol,
  socket,
  socketToHandle,
 )
import System.Directory (doesFileExist, removeFile)
import System.Environment (getExecutablePath)
import System.FileLock (
  SharedExclusive (Exclusive),
  tryLockFile,
  unlockFile,
  withFileLock,
 )
import System.IO (
  Handle,
  IOMode (AppendMode, ReadWriteMode),
  hClose,
  hGetLine,
  openFile,
  stderr,
 )
import System.Process (
  CreateProcess (..),
  StdStream (CreatePipe, NoStream, UseHandle),
  createProcess,
  getPid,
  proc,
 )
import System.Timeout (timeout)

import AgdaMCP.Interact.Daemon.Key (Key (..), Paths (..), preparePaths)
import AgdaMCP.Interact.Daemon.Protocol (
  BuildIdentity,
  Control (..),
  HelloRequest (..),
  HelloResponse (..),
  Request,
  Response,
  currentBuild,
  receive,
  send,
 )
import AgdaMCP.Options (renderFlags)

{- | The daemon could not be reached or did not answer: nothing was answered,
so the client reports the text and exits 2.
-}
newtype Plumbing = Plumbing Text
  deriving (Show)

instance Exception Plumbing

{- | Connect to the daemon's socket, without starting one. The handle is the
connection: every line of the exchange goes through this one, since a second
handle on the same socket would buffer separately and could consume the
other's input.
-}
openConnection :: Paths -> IO Handle
openConnection paths = do
  connection <- socket AF_UNIX Stream defaultProtocol
  connect connection $ SockAddrUnix $ socketPath paths
  socketToHandle connection ReadWriteMode

{- | Send the hello and read the reply. @Nothing@ means the daemon closed
before replying and the request never started, so the caller may retry once.
-}
hello :: Handle -> BuildIdentity -> IO (Maybe HelloResponse)
hello connection build = do
  send connection $ HelloBuild build
  receive connection

-- | Send one request after a successful hello and read its response.
request :: Handle -> Request -> IO Response
request connection body = do
  send connection body
  receive connection >>= maybe (throwIO $ Plumbing exited) pure

-- | Send a control line in place of the hello and read its response.
control :: Handle -> Control -> IO Response
control connection command = do
  send connection $ HelloControl command
  receive connection >>= maybe (throwIO $ Plumbing exited) pure

exited :: Text
exited = "agda-interact daemon exited during the request"

{- | Run an action against the daemon for the key, starting one if none is
listening and replacing one whose build differs from this process's. Takes an
idle timeout in seconds for the daemon spawn. The handle has exchanged the
hello, so the action sends the request.
-}
withDaemon :: Key -> Int -> (Handle -> IO a) -> IO a
withDaemon key idleTimeout body = do
  paths <- preparePaths key
  build <- currentBuild
  let handshake retry replace = do
        connection <- connectOrSpawn key paths idleTimeout
        reply <- hello connection build `onException` hClose connection
        case reply of
          Just BuildOk -> pure connection
          -- EOF before the reply: the daemon closed its listener between our
          -- connect and its accept, so the request never started.
          Nothing
            | retry -> hClose connection >> handshake False replace
            | otherwise -> do
                hClose connection
                throwIO $ Plumbing "agda-interact daemon closed the connection before answering"
          Just (BuildMismatch _)
            | replace -> do
                hClose connection
                stopAndAwait key paths
                handshake retry False
            | otherwise -> do
                hClose connection
                throwIO $
                  Plumbing
                    "the agda-interact daemon that replaced the old one is a different build again"
  bracket (handshake True True) hClose $ \connection ->
    body connection `catch` \(e :: IOException) ->
      throwIO $ Plumbing $ Text.pack (displayException e) `seeing` paths

{- | Run an action on a fresh connection to the daemon for the key, starting
one if none is listening, without the hello. For the controls, which the daemon
honours regardless of build.
-}
withConnection :: Key -> Int -> (Handle -> IO a) -> IO a
withConnection key idleTimeout body = do
  paths <- preparePaths key
  bracket (connectOrSpawn key paths idleTimeout) hClose $ \connection ->
    body connection `catch` \(e :: IOException) ->
      throwIO $ Plumbing $ Text.pack (displayException e) `seeing` paths

{- | Run an action on a fresh connection to the daemon for the key if one is
listening, and answer @Nothing@ without starting one otherwise.
-}
withRunning :: Key -> (Handle -> IO a) -> IO (Maybe a)
withRunning key body = do
  paths <- preparePaths key
  opened <- try $ openConnection paths
  case opened of
    Left (_ :: IOException) -> pure Nothing
    Right connection ->
      Just <$> bracket (pure connection) hClose body

seeing :: Text -> Paths -> Text
seeing message paths = message <> "; see " <> Text.pack (logPath paths)

{- | Decision 2's connect protocol: connect; failing that, take the spawn lock
and connect again, since a client that lost the race finds the daemon the
winner started; still failing, prove no daemon is alive by taking its lock,
then spawn one and wait for it to say it is ready.
-}
connectOrSpawn :: Key -> Paths -> Int -> IO Handle
connectOrSpawn key paths idleTimeout = do
  first <- try $ openConnection paths
  case first of
    Right connection -> pure connection
    Left (_ :: IOException) ->
      withFileLock (spawnLockPath paths) Exclusive $ \_ -> do
        deadline <- (+ 10) <$> getMonotonicTime
        let attempt = do
              again <- try $ openConnection paths
              case again of
                Right connection -> pure connection
                Left (_ :: IOException) -> do
                  free <- tryLockFile (daemonLockPath paths) Exclusive
                  case free of
                    Just lock -> do
                      unlockFile lock
                      removeIfPresent $ socketPath paths
                      spawn key paths idleTimeout
                      openConnection paths `catch` \(e :: IOException) ->
                        throwIO $
                          Plumbing $
                            ( "agda-interact daemon said it was ready but cannot be reached: "
                                <> Text.pack (displayException e)
                            )
                              `seeing` paths
                    -- A daemon holds its lock but takes no connections: it is
                    -- starting under someone else's hand, or draining before
                    -- exit. Either resolves shortly.
                    Nothing -> do
                      now <- getMonotonicTime
                      if now > deadline
                        then
                          throwIO $
                            Plumbing $
                              "an agda-interact daemon holds the lock for this project but is not accepting connections"
                                `seeing` paths
                        else threadDelay 100000 >> attempt
        attempt

{- | Start a daemon for the key in the background and wait for its readiness
line on the pipe it inherits. Its cwd is the project root, so it computes the
same key from its own working directory as the client did from its own.
-}
spawn :: Key -> Paths -> Int -> IO ()
spawn key paths idleTimeout = do
  executable <- getExecutablePath
  logHandle <- openFile (logPath paths) AppendMode
  (_, out, _, process) <-
    createProcess
      (proc executable arguments)
        { cwd = Just $ keyRoot key
        , new_session = True
        , std_in = NoStream
        , std_out = CreatePipe
        , std_err = UseHandle logHandle
        , close_fds = True
        }
  hClose logHandle
  pipe <-
    maybe
      (throwIO $ Plumbing "the daemon's readiness pipe was not created")
      pure
      out
  ready <-
    ( timeout readinessDeadline (try (hGetLine pipe)) ::
        IO (Maybe (Either IOException String))
    )
      `finally` hClose pipe
  case ready of
    Just (Right "ready") -> do
      pid <- getPid process
      Text.IO.hPutStrLn stderr $
        "started agda-interact daemon for "
          <> Text.pack (keyRoot key)
          <> maybe "" (\p -> " (pid " <> Text.pack (show p) <> ")") pid
    Just _ -> do
      trailer <- logTail paths
      throwIO $
        Plumbing $
          "agda-interact daemon exited before it was ready" <> trailer
    Nothing ->
      throwIO $
        Plumbing $
          "agda-interact daemon did not become ready within 10 seconds" `seeing` paths
 where
  arguments =
    renderFlags (keyFlags key)
      <> ["--idle-timeout", show idleTimeout <> "s", "daemon"]

readinessDeadline :: Int
readinessDeadline = 10 * 1000000

-- | The last lines of the daemon's log, for a spawn that died.
logTail :: Paths -> IO Text
logTail paths = do
  present <- doesFileExist $ logPath paths
  if not present
    then pure ""
    else do
      contents <- Text.IO.readFile $ logPath paths
      let trailing = reverse $ take 20 $ reverse $ Text.lines contents
      pure $
        if null trailing
          then ""
          else
            "; the end of " <> Text.pack (logPath paths) <> ":\n" <> Text.unlines trailing

{- | Decision 3's replace: ask the mismatched daemon to stop, wait for its lock
to come free, and say so once. The spawn that follows takes the lock.
-}
stopAndAwait :: Key -> Paths -> IO ()
stopAndAwait key paths = do
  connection <- openConnection paths
  _ <- control connection ControlStop `finally` hClose connection
  deadline <- (+ 10) <$> getMonotonicTime
  let await = do
        free <- tryLockFile (daemonLockPath paths) Exclusive
        case free of
          Just lock -> unlockFile lock
          Nothing -> do
            now <- getMonotonicTime
            if now > deadline
              then
                throwIO $
                  Plumbing $
                    "the agda-interact daemon of another build did not exit within 10 seconds of being asked to"
                      `seeing` paths
              else threadDelay 100000 >> await
  await
  Text.IO.hPutStrLn stderr $
    "restarted agda-interact daemon for "
      <> Text.pack (keyRoot key)
      <> ": build changed"

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
  present <- doesFileExist path
  if present then removeFile path else pure ()
