module Main (main) where

import System.Exit (die)

import AgdaMCP (load, sendCommand, startWorker)

main :: IO ()
main = do
  worker <- startWorker
  result1 <- sendCommand worker $ load "examples/Hole.agda"
  case result1 of
    Left e -> die e
    Right _ -> pure ()
  result2 <- sendCommand worker $ load "examples/Hole.agda"
  case result2 of
    Left e -> die e
    Right _ -> pure ()
