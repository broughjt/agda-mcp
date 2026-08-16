module Main (main) where

import Control.Concurrent.MVar (newMVar)
import Data.Text.IO qualified as Text.IO
import MCP.Server (runMcpServerStdio)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stderr)

import AgdaMCP (handlers, newToolState, serverInfo)
import AgdaMCP.Tools.Options (Outcome (Helped, Parsed, Rejected), parseOptions)

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
