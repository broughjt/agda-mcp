{-# LANGUAGE OverloadedStrings #-}

-- Shared fixture, session, and assertion helpers for the integration suite's
-- per-tool test modules (`Load`, `Give`, and future `goal`/`constraints`/
-- `make_case` modules).
module Common (
  expectLoaded,
  expectLoadFailed,
  expectRejected,
  runSession,
  spanCoordinates,
  withFixture,
  withFixtureDirectory,
  withHoleGiven,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Directory (copyFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.HUnit (assertFailure)

import Agda.Interaction.Command (CommandM)

import AgdaMCP.Position (Position (..), Span (..))
import AgdaMCP.Session (newSession, runCommandM)
import AgdaMCP.Tools.Common (AgdaError, NonFatalError, Warning)
import AgdaMCP.Tools.Give (GiveOutcome (..), GiveRejection)
import AgdaMCP.Tools.Load (Goal, HiddenMetavariable, LoadResponse (..))

-- Copy a fixture into a fresh temporary directory and hand its new path to
-- the test. The basename is kept because Agda checks the module name
-- against the file name.
withFixture :: FilePath -> (FilePath -> IO a) -> IO a
withFixture name action =
  withFixtureDirectory [name] $ \directory -> action (directory </> name)

-- Copy a group of fixtures into one fresh temporary directory. This is used
-- for import scenarios, where Agda must be able to resolve sibling modules.
withFixtureDirectory :: [FilePath] -> (FilePath -> IO a) -> IO a
withFixtureDirectory names action =
  withSystemTempDirectory "agda-mcp-integration" $ \directory -> do
    mapM_
      (\name -> copyFile ("test" </> "fixtures" </> name) (directory </> name))
      names
    action directory

-- We use one session per test. Multi-step scenarios use a single session, which
-- is exactly how the server works in production.
runSession :: CommandM a -> IO a
runSession action = newSession >>= fmap fst . runCommandM action

expectLoaded ::
  LoadResponse ->
  IO ([Goal], [HiddenMetavariable], [Warning], [NonFatalError])
expectLoaded (Loaded goals metas warnings errors) =
  pure (goals, metas, warnings, errors)
expectLoaded other = assertFailure ("expected Loaded, got " <> show other)

expectLoadFailed :: LoadResponse -> IO AgdaError
expectLoadFailed (LoadFailed e) = pure e
expectLoadFailed other = assertFailure ("expected LoadFailed, got " <> show other)

expectRejected :: GiveOutcome -> IO GiveRejection
expectRejected (GiveRejected rejection) = pure rejection
expectRejected other = assertFailure ("expected GiveRejected, got " <> show other)

spanCoordinates :: Span -> ((Int, Int), (Int, Int))
spanCoordinates s =
  ( (positionLine (spanStart s), positionColumn (spanStart s))
  , (positionLine (spanEnd s), positionColumn (spanEnd s))
  )

withHoleGiven :: Text -> ByteString -> ByteString
withHoleGiven expression = encodeUtf8 . Text.replace "{!!}" expression . decodeUtf8
