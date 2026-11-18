module Test.Tool.Harness (
  withFixtureToolSession,
  withFixtureToolSessions,
  withFixtureToolState,
) where

import AgdaMCP.Interaction (InteractionState)
import Control.Monad.State (runStateT)
import System.FilePath (takeFileName, (</>))

import AgdaMCP.Tools.LoadId (LoadGeneration (..))
import AgdaMCP.Tools.State (ToolM, ToolState (..))
import Test.Harness (withStagedFiles)

withFixtureToolSession ::
  IO InteractionState -> FilePath -> (FilePath -> ToolM a) -> IO a
withFixtureToolSession warm source k =
  withFixtureToolSessions warm [source] $ \paths ->
    case paths of
      [path] -> k path
      _ -> error "withFixtureToolSessions answered a different length"

withFixtureToolSessions ::
  IO InteractionState -> [FilePath] -> ([FilePath] -> ToolM a) -> IO a
withFixtureToolSessions warm sources k =
  withFixtureToolState warm sources $ \paths toolState ->
    fst <$> runStateT (k paths) toolState

withFixtureToolState ::
  IO InteractionState -> [FilePath] -> ([FilePath] -> ToolState -> IO a) -> IO a
withFixtureToolState warm sources k =
  withStagedFiles sources $ \directory -> do
    interactionState <- warm
    k
      (map ((directory </>) . takeFileName) sources)
      ToolState
        { toolInteractionState = interactionState
        , toolLoadGeneration =
            LoadGeneration {loadsIssued = 0, currentLoad = Nothing}
        }
