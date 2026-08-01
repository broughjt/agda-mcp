module Test.Tool.Harness (
  withFixtureToolSession,
) where

import Agda.TypeChecking.Monad (TCState)
import Control.Monad.State (runStateT)
import System.FilePath (takeFileName, (</>))

import AgdaMCP.Tools.LoadId (LoadGeneration (..))
import AgdaMCP.Tools.State (ToolM, ToolState (..))
import Test.Harness (warmedSession, withStagedFiles)

withFixtureToolSession ::
  IO TCState -> FilePath -> (FilePath -> ToolM a) -> IO a
withFixtureToolSession warm source k =
  withStagedFiles [source] $ \directory options -> do
    interactionState <- warmedSession warm options
    let toolState =
          ToolState
            { toolInteractionState = interactionState
            , toolLoadGeneration =
                LoadGeneration {loadsIssued = 0, currentLoad = Nothing}
            }
    fst <$> runStateT (k $ directory </> takeFileName source) toolState
