module AgdaMCP.Session (
  Command (..),
  ProtocolViolation (..),
  Session,
  SessionM,
  newSession,
  runCommand,
  runCommandM,
) where

import Agda.Interaction.Base (
  CommandQueue (..),
  CommandState (..),
  IOTCM' (..),
  initCommandState,
 )
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
import Agda.Syntax.Position (Range)
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
import Control.Monad.State (StateT (..), evalStateT, lift)
import Data.Aeson (Value)
import Data.Bitraversable (bitraverse)
import Data.IORef (modifyIORef', newIORef, readIORef)

-- A command paired with the parser for its list of responses. We do it this way
-- because the parser needs to run in the typechecking monad.
data Command r = Command
  { commandIOTCM :: IOTCM' Range
  , commandParse :: [Response] -> TCM (Either (ProtocolViolation Response) r)
  }

-- The list of responses didn't match our mental model of the given command.
data ProtocolViolation a = ProtocolViolation
  { violationCommand :: String
  , violationResponses :: [a]
  }
  deriving (Foldable, Functor, Show, Traversable)

-- A protocol violation is a bug in agda-mcp. `runCommand` throws it and we
-- deliberately catch it nowhere: it propagates out of the tool handler,
-- through the transport loop, and kills the process with the encoded
-- exchange dumped to stderr.
instance Exception (ProtocolViolation Value)

-- An Agda session as a value, consisting of the type-checker state paired with
-- the interaction-level command state. Each `runCommand` threads them through
-- one `runTCM`, in the same way Agda's own REPL loop does per command
-- (`maybeAbort`, InteractionTop.hs:305-322).
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

-- Interpret one command against the session, returning the parsed result and
-- the successor session. The output callback and its collector are fresh per
-- call (the callback stored in the resulting TCState closes over this call's
-- collector, but the next call overwrites it before running anything). The
-- parser runs inside the same `runTCM`, so it sees the post-command TCState.
--
-- A `ProtocolViolation` is thrown, not returned (see its Exception
-- instance). Since the successor session is only produced on success, the
-- caller's retained session after a violation is the pre-command one.

-- Interpret one command against the session, returning the parsed result and
-- the next session.
--
-- The output callback
runCommand :: Command r -> Session -> IO (r, Session)
runCommand command (Session tcState commandState) = do
  collector <- newIORef []
  ((result, commandState'), tcState') <-
    runTCM initEnv tcState $
      flip runStateT commandState $ do
        lift $ setInteractionOutputCallback $ \r ->
          liftIO $ modifyIORef' collector (r :)
        runInteraction $ const $ commandIOTCM command
        responses <- liftIO $ reverse <$> readIORef collector
        parsed <- lift $ commandParse command responses
        -- TODO: Use regular pretty printing (`ofPrettyTCM`) instead of JSON
        -- version?
        -- TODO: Should the bitraverse be `firstA` or something?
        lift $ bitraverse (traverse encodeTCM) pure parsed
  either throwIO (\r -> pure (r, Session tcState' commandState')) result

runCommandM :: Command r -> SessionM r
runCommandM = StateT . runCommand
