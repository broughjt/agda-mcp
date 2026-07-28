{-# LANGUAGE OverloadedStrings #-}

module Test.Harness (
  warmInteractionState,
  warmedSession,
  runSession,
  withFixtureSession,
  withFixtureDirectory,
  withStagedFiles,
) where

import Agda.Interaction.Options (
  CommandLineOptions (..),
  defaultOptions,
 )
import Agda.TypeChecking.Monad (
  PersistentTCState (..),
  TCState (..),
 )
import Control.Monad.State (runStateT)
import System.Directory (
  copyFile,
  listDirectory,
 )
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)

import AgdaMCP.Interaction.Internal (
  InteractionM,
  InteractionState (..),
  newInteractionState,
 )
import AgdaMCP.Interaction.Load qualified as Load
import Control.Exception (throwIO)

-- Cache Agda standard library interface file loading
warmInteractionState :: IO TCState
warmInteractionState =
  withStagedFiles ["test/fixtures/Warmup.agda"] $ \directory options -> do
    state <- newInteractionState options
    let target = directory </> "Warmup.agda"
    (response, InteractionState tcState _ _) <-
      runStateT
        (Load.load Load.Request {Load.requestPath = target, Load.requestArguments = []})
        state
    case response of
      Load.ResponseOk {} -> pure tcState
      other ->
        throwIO $
          userError $
            "The test warm-up fixture failed to load: " <> show other

warmedSession :: IO TCState -> CommandLineOptions -> IO InteractionState
warmedSession warm options = do
  warmState <- warm
  InteractionState tcState commandState slot <- newInteractionState options
  let persistent =
        (stPersistentState warmState)
          { stInteractionOutputCallback =
              stInteractionOutputCallback (stPersistentState tcState)
          }
  pure $
    InteractionState tcState {stPersistentState = persistent} commandState slot

withFixtureSession ::
  IO TCState -> FilePath -> (FilePath -> InteractionM a) -> IO a
withFixtureSession warm source k =
  withStagedFiles [source] $ \directory options ->
    runSession warm options (k (directory </> takeFileName source))

withFixtureDirectory ::
  IO TCState -> FilePath -> (FilePath -> InteractionM a) -> IO a
withFixtureDirectory warm source k = do
  entries <- listDirectory source
  let sources =
        [source </> entry | entry <- entries, takeExtension entry == ".agda"]
  withStagedFiles sources $ \directory options ->
    runSession warm options (k directory)

withStagedFiles ::
  [FilePath] -> (FilePath -> CommandLineOptions -> IO a) -> IO a
withStagedFiles sources k = do
  standardLibrary <- standardLibraryPath
  withSystemTempDirectory "agda-mcp-test" $ \directory -> do
    let librariesFile = directory </> "libraries"
    writeFile librariesFile (standardLibrary </> "standard-library.agda-lib\n")
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
    k directory defaultOptions {optOverrideLibrariesFile = Just librariesFile}

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
runSession :: IO TCState -> CommandLineOptions -> InteractionM a -> IO a
runSession warm options action =
  fst <$> (warmedSession warm options >>= runStateT action)
