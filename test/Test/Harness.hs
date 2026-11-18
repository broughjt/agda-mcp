{-# LANGUAGE OverloadedStrings #-}

module Test.Harness (
  withWarmSession,
  runSession,
  withFixtureSession,
  withFixtureDirectory,
  withStagedFiles,
) where

import Agda.Interaction.Options (
  CommandLineOptions (..),
  defaultOptions,
 )
import Control.Monad.State (runStateT)
import System.Directory (
  copyFile,
  listDirectory,
  removeDirectoryRecursive,
 )
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO.Temp (
  createTempDirectory,
  getCanonicalTemporaryDirectory,
  withSystemTempDirectory,
 )
import Test.Tasty (TestTree, withResource)

import AgdaMCP.Interaction (
  InteractionM,
  InteractionState,
  newInteractionState,
 )
import AgdaMCP.Interaction.Commands.Load qualified as Load
import Control.Exception (throwIO)

withWarmSession :: (IO InteractionState -> TestTree) -> TestTree
withWarmSession k =
  withResource acquire (removeDirectoryRecursive . fst) (k . fmap snd)
 where
  acquire = do
    standardLibrary <- standardLibraryPath
    temporary <- getCanonicalTemporaryDirectory
    directory <- createTempDirectory temporary "agda-mcp-test-warm"
    let librariesFile = directory </> "libraries"
    writeFile librariesFile (standardLibrary </> "standard-library.agda-lib\n")
    stageFiles directory ["test/fixtures/Warmup.agda"]
    state <-
      newInteractionState
        defaultOptions {optOverrideLibrariesFile = Just librariesFile}
    (response, warmed) <-
      runStateT
        ( Load.load
            Load.Request
              { Load.requestPath = directory </> "Warmup.agda"
              , Load.requestArguments = []
              }
        )
        state
    case response of
      Load.ResponseOk {} -> pure (directory, warmed)
      other ->
        throwIO $
          userError $
            "The test warm-up fixture failed to load: " <> show other

withFixtureSession ::
  IO InteractionState -> FilePath -> (FilePath -> InteractionM a) -> IO a
withFixtureSession warm source k =
  withStagedFiles [source] $ \directory ->
    runSession warm (k (directory </> takeFileName source))

withFixtureDirectory ::
  IO InteractionState -> FilePath -> (FilePath -> InteractionM a) -> IO a
withFixtureDirectory warm source k = do
  entries <- listDirectory source
  let sources =
        [source </> entry | entry <- entries, takeExtension entry == ".agda"]
  withStagedFiles sources $ \directory ->
    runSession warm (k directory)

withStagedFiles :: [FilePath] -> (FilePath -> IO a) -> IO a
withStagedFiles sources k =
  withSystemTempDirectory "agda-mcp-test" $ \directory ->
    stageFiles directory sources >> k directory

stageFiles :: FilePath -> [FilePath] -> IO ()
stageFiles directory sources = do
  writeFile
    (directory </> "fixture.agda-lib")
    ( unlines
        [ "name: fixture"
        , "include: ."
        , "depend: standard-library"
        ]
    )
  mapM_
    (\source -> copyFile source (directory </> takeFileName source))
    sources

standardLibraryPath :: IO FilePath
standardLibraryPath =
  lookupEnv "AGDA_MCP_STDLIB"
    >>= maybe
      ( throwIO $
          userError
            "AGDA_MCP_STDLIB is not set. Run the test suite inside \
            \`nix develop`, which exports the pinned Agda standard library."
      )
      pure

-- TODO: Maybe delete this when the tool layer is written and duplicates it
runSession :: IO InteractionState -> InteractionM a -> IO a
runSession warm action = fst <$> (warm >>= runStateT action)
