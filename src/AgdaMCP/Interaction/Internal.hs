module AgdaMCP.Interaction.Internal (
  InteractionState (..),
  InteractionM,
  newInteractionState,
  runCommandM,
  catchTCErr,
) where

import Agda.Interaction.Base (CommandQueue (..), CommandState, initCommandState)
import Agda.Interaction.Command (CommandM)
import Agda.TypeChecking.Monad (
  TCErr (..),
  TCState,
  initEnv,
  initStateIO,
  runTCM,
  setInteractionOutputCallback,
 )
import Control.Concurrent.STM (newTChanIO, newTVarIO)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State (StateT (..))

-- TODO: Update documentation here:
--
-- It turns out that all you need to run Agda commands is `TCState` and
-- `CommandState`. Once you have that, you can run Agda commands in `CommandM`
-- without needing channels, locks, or threading (which is what I was doing
-- before I understood this).

-- TODO: and here

-- An Agda session consists of the type-checker state and the interaction-level
-- command state, held as a value between tool calls. Tool executions run in
-- `CommandM` (`StateT CommandState TCM`).
data InteractionState = InteractionState TCState CommandState

-- TODO: Document this design decision in Main.hs explanation or add one to this module
--
-- Tools can snapshot and restore whole session state, and they can call command
-- wrappers which manipulate it, but they can never inspect or build `Session`s
-- themselves.
type InteractionM = StateT InteractionState IO

newInteractionState :: IO InteractionState
newInteractionState = do
  -- The queue is inert--nothing ever uses it. We just need to pass it because
  -- `CommandState` has a `commandQueue` field which needs to be
  -- initialized. Normally, Agda runs in a separate thread and receives commands
  -- over a channel, but we deliberately avoid that here.
  queue <- CommandQueue <$> newTChanIO <*> newTVarIO Nothing
  tcState <- initStateIO
  -- Set a no-op callback. The default one has `__IMPOSSIBLE__`s
  -- (TypeChecking/Monad/Base.hs:6361).
  ((), tcState') <-
    runTCM initEnv tcState $ setInteractionOutputCallback $ const $ pure ()
  pure $ InteractionState tcState' $ initCommandState queue

-- TODO: Document again, this is stale now:
--
-- Run a `CommandM` action against the session held in `ToolM`, producing the
-- next session state. The trick we're pulling is that `TCM` is actually not a
-- state monad, but instead uses an `IORef TCState`. The `runTCM` form creates
-- a new `IORef` in each call so that we can treat the state as a value
-- outside that scope.
runCommandM :: CommandM a -> InteractionM a
runCommandM action = StateT $ \(InteractionState tcState commandState) -> do
  ((result, commandState'), tcState') <-
    runTCM initEnv tcState $ runStateT action commandState
  pure (result, InteractionState tcState' commandState')

{- | Catch a 'TCErr'.

`PatternErr` is exempt, since it is used for an Agda-internal backtracking
control flow mechanism. One of these escaping to us would be a bug in Agda
code, which we should treat as a bug in our code and die loudly.
-}
catchTCErr :: CommandM a -> (TCErr -> CommandM a) -> CommandM a
catchTCErr action handler = action `catchError` handler'
 where
  handler' e@PatternErr {} = throwError e
  handler' e = handler e
