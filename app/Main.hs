module Main (main) where

import AgdaMCP.Server (runServer)
import AgdaMCP.Worker (startWorker)

main :: IO ()
main = startWorker >>= runServer
