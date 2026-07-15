module AgdaMCP.Session (
  Session,
  SessionM,
  liftCommandM,
  liftTCM,
  newSession,
  runInteractionM,
) where

import Agda.Interaction.Base (
  CommandQueue (..),
  CommandState (..),
  IOTCM,
  initCommandState,
 )
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (runInteraction)
import Agda.Interaction.Response (Response)
import Agda.TypeChecking.Monad (
  TCM,
  setInteractionOutputCallback,
 )
import Agda.TypeChecking.Monad.Base (
  TCState,
  initEnv,
  initStateIO,
  runTCM,
 )
import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT (..), lift, runStateT)
import Data.IORef (modifyIORef', newIORef, readIORef)

-- An Agda session as a value, consisting of the type-checker state paired with
-- the interaction-level command state. `liftCommandM` threads them through one
-- `runTCM`, in the same way Agda's own REPL loop does per command
-- (`maybeAbort`, InteractionTop.hs:305-322).

-- It turns out all you need to run Agda commands (in the TCM monad) is
-- `TCState` and `CommandState`. Once you have that, you can run Agda commands
-- without needing channels, locks, or threading (which is what I was doing
-- before I understood this).
data Session = Session TCState CommandState

type SessionM = StateT Session IO

newSession :: IO Session
newSession = do
  -- The queue is inert--nothing ever uses it. We just need to pass it because
  -- `CommandState` has a `commandQueue` field which needs to be
  -- initialized. Normally, Agda runs in a separate thread and receives commands
  -- over a channel, but we deliberately avoid that here.
  queue <- CommandQueue <$> newTChanIO <*> newTVarIO Nothing
  tcState <- initStateIO
  pure $ Session tcState $ initCommandState queue

liftCommandM :: CommandM a -> SessionM a
liftCommandM action = StateT $ \(Session tcState commandState) ->
  ( \((result, commandState'), tcState') ->
      (result, Session tcState' commandState')
  )
    <$> runTCM initEnv tcState (runStateT action commandState)

liftTCM :: TCM a -> SessionM a
liftTCM = liftCommandM . lift

-- Run one typed interaction command and collect every response it emits.
runInteractionM :: IOTCM -> SessionM [Response]
runInteractionM command = do
  collector <- liftIO $ newIORef []
  liftCommandM $ do
    lift $ setInteractionOutputCallback $ \response ->
      liftIO $ modifyIORef' collector (response :)
    runInteraction command
  liftIO $ reverse <$> readIORef collector
