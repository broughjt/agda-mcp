{-# LANGUAGE OverloadedStrings #-}

module Test.Harness (
  runSession,
  withFixtureSession,
  currentFile,
  expectLoaded,
  expectLoadError,
  spanCoordinates,
  spanText,
) where

import Agda.Interaction.Base (
  CommandState (..),
  CurrentFile (..),
 )
import Agda.Interaction.Options (
  CommandLineOptions (..),
  defaultOptions,
 )
import Agda.Utils.FileName (filePath)
import Control.Monad.State (gets, runStateT)
import System.Directory (copyFile)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.HUnit (assertFailure)

import AgdaMCP.Interaction.Internal (
  InteractionM,
  InteractionState (..),
  newInteractionState,
 )
import AgdaMCP.Interaction.Load (Response (..))
import AgdaMCP.Interaction.Model (
  Error,
  Goal,
  HiddenMetavariable,
  NonFatalError,
  Position (..),
  Span (..),
  Warning,
 )
import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Text qualified as Text

withFixtureSession :: FilePath -> (FilePath -> InteractionM a) -> IO a
withFixtureSession source k = do
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
    let staged = directory </> takeFileName source
    copyFile source staged
    runSession
      defaultOptions {optOverrideLibrariesFile = Just librariesFile}
      (k staged)

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
runSession :: CommandLineOptions -> InteractionM a -> IO a
runSession options action =
  fst <$> (newInteractionState options >>= runStateT action)

currentFile :: InteractionM (Maybe FilePath)
currentFile =
  gets $ \(InteractionState _ commandState _) ->
    filePath . currentFilePath <$> theCurrentFile commandState

expectLoaded ::
  String ->
  Response ->
  IO ([Goal], [HiddenMetavariable], [Warning], [NonFatalError])
expectLoaded _ (ResponseOk goals hiddenMetavariables warnings nonFatalErrors) =
  pure (goals, hiddenMetavariables, warnings, nonFatalErrors)
expectLoaded label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectLoadError :: String -> Response -> IO Error
expectLoadError _ (ResponseError e) = pure e
expectLoadError label other =
  assertFailure $ label <> ": expected ResponseError, got " <> show other

spanCoordinates :: Span -> ((Int, Int), (Int, Int))
spanCoordinates s =
  (coordinates (spanStart s), coordinates (spanEnd s))
 where
  coordinates p = (positionLine p, positionColumn p)

spanText :: Text -> Span -> Text
spanText source s =
  Text.take (end - start) (Text.drop start source)
 where
  start = positionOffset $ spanStart s
  end = positionOffset $ spanEnd s
