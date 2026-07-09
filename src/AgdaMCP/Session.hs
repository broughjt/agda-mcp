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

-- A protocol violation is a bug in agda-mcp. `fromProtocolResult` throws it and we
-- deliberately catch it nowhere: it propagates out of the tool handler,
-- through the transport loop, and kills the process with the encoded
-- exchange dumped to stderr.
instance Exception (ProtocolViolation Value)

-- An Agda session as a value, consisting of the type-checker state paired with
-- the interaction-level command state. `liftCommandM` threads them through one
-- `runTCM`, in the same way Agda's own REPL loop does per command (`maybeAbort`,
-- InteractionTop.hs:305-322).
data Session = Session TCState CommandState

type SessionM = StateT Session IO

newSession :: IO Session
newSession = do
  -- The queue is inert; nothing ever reads it. We just need to pass it because
  -- `CommandState` has a `commandQueue` field. Normally, it is Agda's REPL-loop
  -- delivery and Cmd_abort mechanism, but we purposefully don't use either of
  -- these.
  queue <- CommandQueue <$> newTChanIO <*> newTVarIO Nothing
  state <- initStateIO
  (commandState, tcState) <- runTCM initEnv state $ do
    handleCommand_ (lift $ setCommandLineOptions defaultOptions)
      `evalStateT` initCommandState queue
    options <- commandLineOptions
    pure
      (initCommandState queue)
        { optionsOnReload = options {optAbsoluteIncludePaths = []}
        }
  pure (Session tcState commandState)

-- Lift Agda's interaction monad into a session computation, threading both
-- pieces of persistent state through the action.
liftCommandM :: CommandM a -> SessionM a
liftCommandM action = StateT $ \(Session tcState commandState) ->
  ( \((result, commandState'), tcState') ->
      (result, Session tcState' commandState')
  )
    <$> runTCM initEnv tcState (runStateT action commandState)

-- Lift a plain type-checking computation. It can inspect and update the
-- session's TCState while leaving its CommandState unchanged.
liftTCM :: TCM a -> SessionM a
liftTCM = liftCommandM . lift

-- Run one typed interaction command and collect every response it emits. The
-- callback and collector are fresh per call. The callback stored in the
-- resulting TCState closes over this call's collector, but the next call
-- overwrites it before running anything.
runInteractionM :: IOTCM -> SessionM [Response]
runInteractionM command = do
  collector <- liftIO $ newIORef []
  liftCommandM $ do
    lift $ setInteractionOutputCallback $ \response ->
      liftIO $ modifyIORef' collector (response :)
    runInteraction command
  liftIO $ reverse <$> readIORef collector

-- Eliminate the result of a response parser or resolver. A mismatch is
-- encoded while the post-command TCState is current, then thrown uncaught as
-- a bug in agda-mcp.
fromProtocolResult ::
  Either (ProtocolViolation Response) a -> SessionM a
fromProtocolResult = either throwProtocolViolation pure
 where
  throwProtocolViolation violation =
    -- TODO: Use regular pretty printing (`ofPrettyTCM`) instead of JSON
    -- version?
    liftTCM (traverse encodeTCM violation) >>= liftIO . throwIO
