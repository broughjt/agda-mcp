module AgdaMCP (
  startWorker,
  sendCommand,
  load,
) where

import Agda.Interaction.Base (
  Command,
  Command' (..),
  CommandState (..),
  IOTCM' (..),
  Interaction' (..),
  initCommandState,
 )
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (
  handleCommand_,
  initialiseCommandQueue,
  maybeAbort,
  runInteraction,
 )
import Agda.Interaction.Options (
  CommandLineOptions (..),
  commandLineOptions,
  defaultOptions,
 )
import Agda.Interaction.Response (Response)
import Agda.TypeChecking.Monad (
  HighlightingLevel (..),
  HighlightingMethod (..),
  setCommandLineOptions,
  setInteractionOutputCallback,
 )
import Agda.TypeChecking.Monad.Base (runTCMTop)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (
  MVar,
  newEmptyMVar,
  newMVar,
  putMVar,
  takeMVar,
  withMVar,
 )
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT, lift)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef)

data Worker = Worker
  { workerLock :: MVar ()
  , workerCommands :: Chan Command
  , workerResponse :: MVar (Either String [Response])
  }

startWorker :: IO Worker
startWorker = do
  workerCommands <- newChan
  workerResponse <- newEmptyMVar
  workerLock <- newMVar ()
  collector <- newIORef []
  let workerLoop :: CommandM ()
      workerLoop = do
        result <- maybeAbort runInteraction
        responses <- liftIO $ atomicModifyIORef' collector (\rs -> ([], reverse rs))
        case result of
          Done -> pure ()
          Error e -> do
            liftIO $ putMVar workerResponse (Left e)
            workerLoop
          Command _ -> do
            liftIO $ putMVar workerResponse (Right responses)
            workerLoop
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
    case result of
      Left e -> putMVar workerResponse (Left (show e))
      Right (Left e) -> putMVar workerResponse (Left (show e))
      Right (Right ()) -> pure ()
  pure Worker {workerLock, workerCommands, workerResponse}

sendCommand :: Worker -> Command -> IO (Either String [Response])
sendCommand worker command = withMVar (workerLock worker) $ \() ->
  writeChan (workerCommands worker) command
    >> takeMVar (workerResponse worker)

load :: FilePath -> Command
load path = Command $ const $ IOTCM path None Direct (Cmd_load path [])
