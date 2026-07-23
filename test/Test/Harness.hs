{-# LANGUAGE OverloadedStrings #-}

module Test.Harness (
  runSession,
  withFixtureSession,
) where

import Agda.Interaction.Options (
  CommandLineOptions (..),
  defaultOptions,
 )
import Control.Monad.State (runStateT)
import System.Directory (copyFile)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)

import AgdaMCP.Interaction.Internal (InteractionM, newInteractionState)

withFixtureSession :: FilePath -> (FilePath -> InteractionM a) -> IO a
withFixtureSession source k = do
  standardLibrary <- standardLibraryPath
  withSystemTempDirectory "agda-mcp-test" $ \directory -> do
    let librariesFile = directory </> "libraries"
    writeFile librariesFile (standardLibrary </> "standard-library.agda-lib\n")
    writeFile
      (directory </> "fixture.agda-lib")
      (unlines ["name: fixture", "include: .", "depend: standard-library"])
    let staged = directory </> takeFileName source
    copyFile source staged
    runSession
      defaultOptions {optOverrideLibrariesFile = Just librariesFile}
      (k staged)

standardLibraryPath :: IO FilePath
standardLibraryPath =
  lookupEnv "AGDA_MCP_STDLIB"
    >>= maybe
      ( fail
          "AGDA_MCP_STDLIB is not set. Run the test suite inside `nix develop`, \
          \which exports the pinned Agda standard library."
      )
      pure

runSession :: CommandLineOptions -> InteractionM a -> IO a
runSession options action =
  fst <$> (newInteractionState options >>= runStateT action)
