module AgdaMCP.Interaction.Internal (
  InteractionState (..),
  InteractionM,
  GiveSlot,
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
import Agda.Interaction.Response (
  GiveResult,
  Response,
  Response_boot (Resp_GiveAction),
 )
import Agda.Syntax.Common (InteractionId)
import Agda.TypeChecking.Monad (
  TCErr (..),
  TCM,
  TCState,
  initEnv,
  initStateIO,
  runTCM,
  setInteractionOutputCallback,
 )
import Control.Concurrent.STM (newTChanIO, newTVarIO)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT (..))
import Data.IORef (IORef, newIORef, writeIORef)

{- | The state which comprises an Agda session (`TCState` and `CommandState`),
together with an added mutable reference to facilitate the give family of
command wrappers.

The constructor is deliberately hidden to ensure that callers cannot manipulate
Agda state outside of calling the command wrappers exposed here
(e.g. `AgdaMCP.Interaction.Load.load`, `AgdaMCP.Interaction.Give.give`,
`AgdaMCP.Interaction.MakeCase.makeCase`, etc.).
-}
data InteractionState = InteractionState TCState CommandState GiveSlot

-- See @captureGiveAction@.
type GiveSlot = IORef (Maybe (InteractionId, GiveResult))

type InteractionM = StateT InteractionState IO

newInteractionState :: CommandLineOptions -> IO InteractionState
newInteractionState options = do
  -- The queue is inert--nothing ever uses it. We just need to pass it because
  -- @CommandState@ has a @commandQueue@ field which needs to be
  -- initialized. Normally, Agda runs in a separate thread and receives commands
  -- over a channel, but we deliberately avoid that here.
  queue <- CommandQueue <$> newTChanIO <*> newTVarIO Nothing
  slot <- newIORef Nothing
  tcState <- initStateIO
  -- Install the capturing output callback (see @captureGiveAction@).
  ((), tcState') <-
    runTCM initEnv tcState $ setInteractionOutputCallback $ captureGiveAction slot
  -- Clear @optAbsoluteIncludePaths@ the same way that @repl@ does
  -- (AgdaTop.hs:44-46). As far as I understand, clearing this causes library
  -- resolution to run for new loads and prevents the use of stale absolute
  -- paths.
  let commandState =
        (initCommandState queue)
          { optionsOnReload = options {optAbsoluteIncludePaths = []}
          }
  pure $ InteractionState tcState' commandState slot

-- Deliberate hack.
--
-- One goal of the interaction layer is to avoid needing to intercept and parse
-- @Resp_@ streams. However, @give_gen@ is a complicated and delicate looking
-- helper whose logic I don't wish to duplicate, and its output is not written
-- to @TCState@ in any form, so we can't recover the needed information by
-- querying that state after the call. Rather than reimplement @give_gen'@
-- myself I have chosen to use the response output callback in this one
-- instance.
captureGiveAction :: GiveSlot -> Response -> TCM ()
captureGiveAction slot (Resp_GiveAction pointId giveResult) =
  liftIO $ writeIORef slot $ Just (pointId, giveResult)
captureGiveAction _ _ = pure ()

runCommandM :: CommandM a -> InteractionM a
runCommandM action =
  -- The tricky part is that @TCM@ is actually not a state monad, but instead
  -- uses an @IORef TCState@. The @runTCM@ form creates a new @IORef@ in each
  -- call so that we can treat the state as a value outside that scope.
  StateT $ \(InteractionState tcState commandState slot) -> do
    ((result, commandState'), tcState') <-
      runTCM initEnv tcState $ runStateT action commandState
    pure (result, InteractionState tcState' commandState' slot)

-- @PatternErr@ is exempt, since it is used for an Agda-internal backtracking
-- control flow mechanism. One of these escaping would be a bug in Agda code,
-- which we should treat as a bug in our code and die loudly.
catchTCErr :: CommandM a -> (TCErr -> CommandM a) -> CommandM a
catchTCErr action handler = action `catchError` handler'
 where
  handler' e@PatternErr {} = throwError e
  handler' e = handler e
