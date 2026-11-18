module AgdaMCP.Interaction.Internal (
  InteractionState (..),
  InteractionM,
  newInteractionState,
  runCommandM,
  catchTCErr,
) where

import Agda.Interaction.Base (
  CommandQueue (..),
  CommandState (..),
  initCommandState,
 )
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.Options (
  CommandLineOptions (..),
 )
import Agda.TypeChecking.Monad (
  PersistentTCState (..),
  TCErr (..),
  TCState (..),
  initEnv,
  initStateIO,
  runTCM,
  setInteractionOutputCallback,
 )
import Agda.Utils.Atomic (newAtomic, readAtomic)
import Control.Concurrent.STM (newTChanIO, newTVarIO)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.State (StateT (..))

{- | The state which comprises an Agda session (`TCState` and `CommandState`).

The constructor is deliberately hidden to ensure that callers cannot manipulate
Agda state outside of calling the command wrappers exposed here
(e.g. `AgdaMCP.Interaction.Commands.Load.load`, `AgdaMCP.Interaction.Commands.Give.give`,
`AgdaMCP.Interaction.Commands.MakeCase.makeCase`, etc.).
-}
data InteractionState = InteractionState TCState CommandState

type InteractionM = StateT InteractionState IO

newInteractionState :: CommandLineOptions -> IO InteractionState
newInteractionState options = do
  -- The queue is inert--nothing ever uses it. We just need to pass it because
  -- @CommandState@ has a @commandQueue@ field which needs to be
  -- initialized. Normally, Agda runs in a separate thread and receives commands
  -- over a channel, but we deliberately avoid that here.
  queue <- CommandQueue <$> newTChanIO <*> newTVarIO Nothing
  tcState <- initStateIO
  -- Agda's default output callback prints running info to stdout and treats
  -- every other response as impossible, so replace it with one that ignores
  -- responses.
  ((), tcState') <-
    runTCM initEnv tcState $ setInteractionOutputCallback $ const $ pure ()
  -- Clear @optAbsoluteIncludePaths@ the same way that @repl@ does
  -- (AgdaTop.hs). As far as I understand, clearing this causes library
  -- resolution to run for new loads and prevents the use of stale absolute
  -- paths.
  let commandState =
        (initCommandState queue)
          { optionsOnReload = options {optAbsoluteIncludePaths = []}
          }
  pure $ InteractionState tcState' commandState

runCommandM :: CommandM a -> InteractionM a
runCommandM action =
  -- The tricky part is that @TCM@ is actually not a state monad, but instead
  -- uses an @IORef TCState@. The @runTCM@ form creates a new @IORef@ in each
  -- call so that we can treat the state as a value outside that scope.
  --
  -- Since Agda 2.9.0 the session state is a mutable variable shared by every
  -- copy of a @TCState@, so each command runs on a fresh copy of it, keeping
  -- the state it ran from unchanged.
  StateT $ \(InteractionState tcState commandState) -> do
    let persistent = stPersistentState tcState
    session <- newAtomic =<< readAtomic (stPersistentSession persistent)
    let tcState0 =
          tcState {stPersistentState = persistent {stPersistentSession = session}}
    ((result, commandState'), tcState') <-
      runTCM initEnv tcState0 $ runStateT action commandState
    pure (result, InteractionState tcState' commandState')

-- @PatternErr@ is exempt, since it is used for an Agda-internal backtracking
-- control flow mechanism. One of these escaping would be a bug in Agda code,
-- which we should treat as a bug in our code and die loudly.
catchTCErr :: CommandM a -> (TCErr -> CommandM a) -> CommandM a
catchTCErr action handler = action `catchError` handler'
 where
  handler' e@PatternErr {} = throwError e
  handler' e = handler e
