{- | Helpers for the test suite.

Nothing but the test suite should import this module.
-}
module AgdaMCP.Interaction.Testing (
  interactionTCState,
  interactionSession,
  withWarmPersistentState,
  currentFile,
  observeResponses,
) where

import Agda.Interaction.Base (CurrentFile (..), theCurrentFile)
import Agda.Interaction.Response (Response)
import Agda.TypeChecking.Monad (
  PersistentTCState (..),
  SessionState (..),
  TCM,
  TCState (..),
  getInteractionOutputCallback,
  initEnv,
  runTCM,
  setInteractionOutputCallback,
 )
import Agda.Utils.Atomic (newAtomic, readAtomic)
import Agda.Utils.FileName (filePath)
import Control.Monad.State (gets)

import AgdaMCP.Interaction.Internal (
  InteractionM,
  InteractionState (..),
 )

interactionTCState :: InteractionState -> TCState
interactionTCState (InteractionState tcState _ _) = tcState

interactionSession :: InteractionState -> IO SessionState
interactionSession =
  readAtomic . stPersistentSession . stPersistentState . interactionTCState

withWarmPersistentState :: TCState -> InteractionState -> IO InteractionState
withWarmPersistentState warm (InteractionState tcState commandState slot) = do
  warmSession <- readAtomic $ stPersistentSession $ stPersistentState warm
  ownSession <- readAtomic $ stPersistentSession $ stPersistentState tcState
  -- The session state is mutable and shared by every copy of a TCState, so
  -- each session gets its own copy of the warm one, keeping its own callback.
  session <-
    newAtomic
      warmSession
        { stSessionInteractionOutputCallback =
            stSessionInteractionOutputCallback ownSession
        }
  let persistent = (stPersistentState warm) {stPersistentSession = session}
  pure $
    InteractionState tcState {stPersistentState = persistent} commandState slot

currentFile :: InteractionM (Maybe FilePath)
currentFile =
  gets $ \(InteractionState _ commandState _) ->
    filePath . currentFilePath <$> theCurrentFile commandState

observeResponses ::
  (Response -> TCM ()) -> InteractionState -> IO InteractionState
observeResponses observe (InteractionState tcState commandState slot) = do
  ((), tcState') <-
    runTCM initEnv tcState $ do
      installed <- getInteractionOutputCallback
      setInteractionOutputCallback $
        \response -> installed response >> observe response
  pure $ InteractionState tcState' commandState slot
