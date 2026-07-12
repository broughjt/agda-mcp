module AgdaMCP.Session (
  ProtocolViolation (..),
  Session,
  SessionM,
  fromProtocolResult,
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
import Agda.Interaction.InteractionTop (
  handleCommand_,
  runInteraction,
 )
import Agda.Interaction.JSON (EncodeTCM (..))
import Agda.Interaction.JSONTop ()
import Agda.Interaction.Options (
  CommandLineOptions (..),
  commandLineOptions,
  defaultOptions,
 )
import Agda.Interaction.Response (Response)
import Agda.TypeChecking.Monad (
  TCM,
  setCommandLineOptions,
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
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT (..), evalStateT, lift, runStateT)
import Data.Aeson (Value)
import Data.IORef (modifyIORef', newIORef, readIORef)

-- The list of responses didn't match our mental model of the given command.
data ProtocolViolation a = ProtocolViolation
  { violationCommand :: String
  , violationResponses :: [a]
  }
  deriving (Foldable, Functor, Show, Traversable)

-- A `ProtocolViolation` is always a bug in agda-mcp. If we encounter one we
-- should not attempt to recover, but instead die loudly with good debugging
-- information.
instance Exception (ProtocolViolation Value)

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

-- Helper for throwing on a potential `ProtocolViolation`, which should be
-- treated as fatal.
fromProtocolResult ::
  Either (ProtocolViolation Response) a -> SessionM a
fromProtocolResult = either throwProtocolViolation pure
 where
  throwProtocolViolation violation =
    liftTCM (traverse encodeTCM violation) >>= liftIO . throwIO
