module Main (main) where

import Data.Text.IO qualified as Text.IO
import MCP.Server (initMCPServerState, serveStdio)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdin, stdout)

import AgdaMCP (
  capabilities,
  handlers,
  implementation,
  instructions,
  newToolState,
 )
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
  toolState <- newToolState options
  serveStdio stdin stdout $
    initMCPServerState
      toolState
      Nothing
      Nothing
      capabilities
      implementation
      (Just instructions)
      handlers
