{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Runtime
    ( AgdaRuntime
    , startAgdaRuntime
    , stopAgdaRuntime
    , agdaLoad
    , agdaGive
    ) where

import AgdaMCP.Types
    ( AgdaError (AgdaNotImplemented, AgdaRuntimeStopped)
    , GiveResult
    , LoadResult
    )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)

-- | Handle to the persistent Agda interaction runtime.
--
-- Phase 1 keeps this intentionally small. Phase 3 will replace the stub with a
-- background Agda interaction loop, command queue, response accumulator, and
-- source-edit extraction.
newtype AgdaRuntime = AgdaRuntime
    { runtimeStopped :: IORef Bool
    }

startAgdaRuntime :: IO AgdaRuntime
startAgdaRuntime = AgdaRuntime <$> newIORef False

stopAgdaRuntime :: AgdaRuntime -> IO ()
stopAgdaRuntime runtime = writeIORef (runtimeStopped runtime) True

agdaLoad :: AgdaRuntime -> FilePath -> IO (Either AgdaError LoadResult)
agdaLoad runtime _filePath = do
    stopped <- readIORef $ runtimeStopped runtime
    pure $
        if stopped
            then Left AgdaRuntimeStopped
            else Left $ AgdaNotImplemented "agdaLoad"

agdaGive :: AgdaRuntime -> FilePath -> Int -> Text -> IO (Either AgdaError GiveResult)
agdaGive runtime _filePath _goalId _expression = do
    stopped <- readIORef $ runtimeStopped runtime
    pure $
        if stopped
            then Left AgdaRuntimeStopped
            else Left $ AgdaNotImplemented "agdaGive"
