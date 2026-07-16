module AgdaMCP.Session (
  Session,
  newSession,
  runCommandM,
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
import Agda.TypeChecking.Monad (setInteractionOutputCallback)
import Agda.TypeChecking.Monad.Base (
  TCState,
  initEnv,
  initStateIO,
  runTCM,
 )
import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift, runStateT)
import Data.IORef (modifyIORef', newIORef, readIORef)

-- It turns out that all you need to run Agda commands is `TCState` and
-- `CommandState`. Once you have that, you can run Agda commands in `CommandM`
-- without needing channels, locks, or threading (which is what I was doing
-- before I understood this).

-- An Agda session consists of the type-checker state and the interaction-level
-- command state, held as a value between tool calls. Tool executions run in
-- `CommandM` (`StateT CommandState TCM`).
data Session = Session TCState CommandState

newSession :: IO Session
newSession = do
  -- The queue is inert--nothing ever uses it. We just need to pass it because
  -- `CommandState` has a `commandQueue` field which needs to be
  -- initialized. Normally, Agda runs in a separate thread and receives commands
  -- over a channel, but we deliberately avoid that here.
  queue <- CommandQueue <$> newTChanIO <*> newTVarIO Nothing
  tcState <- initStateIO
  pure $ Session tcState $ initCommandState queue

-- Run a `CommandM` action against the current session state (just `TCState` and
-- `CommandState`), producing the next session state. The trick we're pulling is
-- that `TCM` is actually not a state monad, but instead uses an `IORef
-- TCState`. The `runTCM` form creates a new `IORef` in each call so that we can
-- treat the state as a value outside that scope.
runCommandM :: CommandM a -> Session -> IO (a, Session)
runCommandM action (Session tcState commandState) = do
  ((result, commandState'), tcState') <-
    runTCM initEnv tcState (runStateT action commandState)
  pure (result, Session tcState' commandState')

-- Run one interaction command and collect the emitted list of responses.
runInteractionM :: IOTCM -> CommandM [Response]
runInteractionM command = do
  collector <- liftIO $ newIORef []
  lift $ setInteractionOutputCallback $ \response ->
    liftIO $ modifyIORef' collector (response :)
  runInteraction command
  liftIO $ reverse <$> readIORef collector
