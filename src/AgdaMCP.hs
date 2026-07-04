module AgdaMCP where

import Control.Concurrent (forkIO)

data WorkerHandle = WorkerHandle { channel :: Int
                                 , lock :: Int
                                 , threadId :: Int
                                 }

startWorker :: IO WorkerHandle
startWorker = -- forkIO $ 
  error "unimplemented"

