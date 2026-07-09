module Main (main) where

import AgdaMCP.Server (runServer)
import AgdaMCP.Session (newSession)

main :: IO ()
main = newSession >>= runServer
