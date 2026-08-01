{- | Helpers for the test suite.

Nothing but the test suite should import this module.
-}
module AgdaMCP.Interaction.Testing (
  interactionTCState,
  withWarmPersistentState,
  currentFile,
  observeResponses,
) where

import Agda.Interaction.Base (CurrentFile (..), theCurrentFile)
import Agda.Interaction.Response (Response)
import Agda.TypeChecking.Monad (
  PersistentTCState (..),
  TCM,
  TCState (..),
  initEnv,
  runTCM,
  setInteractionOutputCallback,
 )
import Agda.Utils.FileName (filePath)
import Control.Monad.State (gets)

import AgdaMCP.Interaction.Internal (
  InteractionM,
  InteractionState (..),
 )

interactionTCState :: InteractionState -> TCState
interactionTCState (InteractionState tcState _ _) = tcState

withWarmPersistentState :: TCState -> InteractionState -> InteractionState
withWarmPersistentState warm (InteractionState tcState commandState slot) =
  InteractionState tcState {stPersistentState = persistent} commandState slot
 where
  persistent =
    (stPersistentState warm)
      { stInteractionOutputCallback =
          stInteractionOutputCallback (stPersistentState tcState)
      }

currentFile :: InteractionM (Maybe FilePath)
currentFile =
  gets $ \(InteractionState _ commandState _) ->
    filePath . currentFilePath <$> theCurrentFile commandState

observeResponses ::
  (Response -> TCM ()) -> InteractionState -> IO InteractionState
observeResponses observe (InteractionState tcState commandState slot) = do
  let installed = stInteractionOutputCallback (stPersistentState tcState)
  ((), tcState') <-
    runTCM initEnv tcState $
      setInteractionOutputCallback $
        \response -> installed response >> observe response
  pure $ InteractionState tcState' commandState slot
