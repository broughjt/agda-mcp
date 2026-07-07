module AgdaMCP.Worker (
  Command (..),
  Failure (..),
  ProtocolViolation (..),
  Worker,
  sendCommand,
  startWorker,
) where

import Agda.Interaction.Base (
  CommandState (..),
  IOTCM' (..),
  initCommandState,
 )
import Agda.Interaction.Base qualified as Agda
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (
  handleCommand_,
  initialiseCommandQueue,
  maybeAbort,
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
import Agda.TypeChecking.Monad.Base (TCErr, runTCMTop)
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (
  MVar,
  newEmptyMVar,
  putMVar,
  readMVar,
  takeMVar,
  tryPutMVar,
 )
import Control.Exception (Exception, SomeException, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT, lift)
import Data.Aeson (Value)
import Data.Bitraversable (bitraverse)
import Data.IORef (
  atomicModifyIORef',
  modifyIORef',
  newIORef,
 )

-- A command paired with the parser for its list of responses. We do it this way
-- because the parser needs to run in the type checking monad.
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

-- Bugs in agda-mcp.
--
-- QueueError: Agda's queue reported a command parse error. Only parseIOTCM
-- produces these, so unreachable with typed IOTCM values.
--
-- WorkerExited: the loop returned normally. This should be unreachable, since
-- nothing sends `Done` into the channel (and Done hits __IMPOSSIBLE__ first).
data Failure
  = ParseViolation (ProtocolViolation Value)
  | QueueError String
  | WorkerException SomeException
  | WorkerTCError TCErr
  | WorkerExited
  deriving (Show)

instance Exception Failure

data Job = forall r. Job (Command r) (MVar (Either Failure r))

data Worker = Worker
  { workerJob :: MVar Job
  , workerCommands :: Chan Agda.Command
  }

startWorker :: IO Worker
startWorker = do
  workerCommands <- newChan
  workerJob <- newEmptyMVar
  collector <- newIORef []
  let workerLoop :: CommandM ()
      workerLoop = do
        result <- maybeAbort runInteraction
        responses <- liftIO $ atomicModifyIORef' collector (\rs -> ([], reverse rs))
        case result of
          Agda.Done -> __IMPOSSIBLE__
          Agda.Error e -> do
            Job _ responseMVar <- liftIO $ readMVar workerJob
            liftIO $ putMVar responseMVar (Left (QueueError e))
          Agda.Command _ -> do
            Job command responseMVar <- liftIO $ readMVar workerJob
            parsed <- lift $ commandParse command responses
            result' <-
              lift $ bitraverse (fmap ParseViolation . traverse encodeTCM) pure parsed
            liftIO $ putMVar responseMVar result'
        liftIO $ void $ takeMVar workerJob
        workerLoop

      -- The worker died, so we repeatedly take each job out of the slot and try
      -- to tell callers we failed.
      rejectJobs :: MVar Job -> Failure -> IO ()
      rejectJobs slot failure =
        forever $ do
          Job _ responseMVar <- takeMVar slot
          void $ tryPutMVar responseMVar (Left failure)
  _ <- forkIO $ do
    result <- try @SomeException $ runTCMTop $ do
      setInteractionOutputCallback $ \r ->
        liftIO $ modifyIORef' collector (r :)
      queue <- liftIO $ initialiseCommandQueue $ readChan workerCommands
      handleCommand_ (lift $ setCommandLineOptions defaultOptions)
        `evalStateT` initCommandState queue
      options <- commandLineOptions
      evalStateT
        workerLoop
        (initCommandState queue)
          { optionsOnReload = options {optAbsoluteIncludePaths = []}
          }
    let failure = case result of
          Left e -> WorkerException e
          Right (Left e) -> WorkerTCError e
          Right (Right ()) -> WorkerExited
    rejectJobs workerJob failure
  pure Worker {workerJob, workerCommands}

-- The job slot doubles as the send lock. The slot stays full while its
-- command is pending or in flight, and the worker empties it only after
-- responding. It follows that these events alternate strictly:
--
--   put job N < enqueue command N < execute N < response N < clear job N < put job N+1
--
-- which gives two invariants at once:
--
-- 1. at most one command is in flight;
-- 2. the N-th executed command is always paired with the N-th job.
sendCommand :: Worker -> Command r -> IO (Either Failure r)
sendCommand worker command = do
  responseMVar <- newEmptyMVar
  putMVar (workerJob worker) (Job command responseMVar)
  writeChan (workerCommands worker) (Agda.Command (const (commandIOTCM command)))
  takeMVar responseMVar
