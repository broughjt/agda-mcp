module Main (main) where

import Control.Concurrent.MVar (newMVar)
import Data.Text.IO qualified as Text.IO
import MCP.Server (runMcpServerStdio)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stderr)

import AgdaMCP.MCP (
  Outcome (Helped, Parsed, Rejected),
  handlers,
  parseOptions,
  serverInfo,
 )
import AgdaMCP.Tools (newToolState)

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  arguments <- getArgs
  options <- case parseOptions arguments of
    Parsed options -> pure options
    Helped text -> do
      Text.IO.putStrLn text
      exitSuccess
    Rejected problem -> do
      Text.IO.hPutStrLn stderr problem
      exitFailure
  stateVariable <- newMVar =<< newToolState options
  runMcpServerStdio serverInfo $ handlers stateVariable
