module Main (main) where

import Control.Concurrent.MVar (newMVar)
import Control.Exception (IOException, catch)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.IO (BufferMode (..), hFlush, hSetBuffering, stderr, stdout)

import AgdaMCP.Interact.CLI (
  Exit (..),
  Invocation (..),
  Mode (..),
  Outcome (..),
  Stream (..),
  answered,
  helped,
  parseCommandLine,
  plumbing,
  rejected,
 )
import AgdaMCP.Interact.Daemon.Client (
  Plumbing (..),
  control,
  request,
  withConnection,
  withDaemon,
  withRunning,
 )
import AgdaMCP.Interact.Daemon.Key (Key (..), newKey, preparePaths)
import AgdaMCP.Interact.Daemon.Protocol (
  Control (..),
  Request (..),
  currentBuild,
 )
import AgdaMCP.Interact.Daemon.Server (Daemon (..), serve)
import AgdaMCP.Options (toCommandLineOptions)
import AgdaMCP.Tools (newToolState)

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  directory <- getCurrentDirectory
  arguments <- getArgs
  case parseCommandLine directory arguments of
    Helped text -> exit $ helped text
    Rejected text -> exit $ rejected text
    Parsed invocation ->
      run directory invocation `catch` \(Plumbing text) -> exit $ plumbing text

run :: FilePath -> Invocation -> IO ()
run directory invocation = do
  key <- newKey directory $ invocationFlags invocation
  case invocationMode invocation of
    RunDaemon -> do
      paths <- preparePaths key
      build <- currentBuild
      stateVariable <-
        newMVar =<< newToolState (toCommandLineOptions $ keyFlags key)
      serve
        Daemon
          { daemonKey = key
          , daemonPaths = paths
          , daemonBuild = build
          , daemonIdleTimeout = idleTimeout
          , daemonReady = announceReady
          }
        stateVariable
    Invoke name arguments ->
      withDaemon key idleTimeout (`request` Request name arguments)
        >>= exit . answered format
    Status ->
      withConnection key idleTimeout (`control` ControlStatus)
        >>= exit . answered format
    -- Stopping what is not running is already done, so no daemon is started
    -- for the purpose.
    Stop ->
      withRunning key (`control` ControlStop)
        >>= maybe
          ( exit $
              Exit
                (Text.pack $ "no agda-interact daemon is running for " <> keyRoot key)
                Stderr
                ExitSuccess
          )
          (exit . answered format)
 where
  idleTimeout = invocationIdleTimeout invocation
  format = invocationFormat invocation

{- | Tell the client that spawned this daemon that the socket is listening. A
client that gave up, or was interrupted, while waiting has closed its end of
the pipe; the daemon it started is still wanted by the next client, so a
failed write is not a reason to exit.
-}
announceReady :: IO ()
announceReady =
  (putStrLn "ready" >> hFlush stdout) `catch` \(_ :: IOException) -> pure ()

exit :: Exit -> IO a
exit (Exit text stream code) = do
  Text.IO.hPutStrLn (case stream of Stdout -> stdout; Stderr -> stderr) text
  exitWith code
