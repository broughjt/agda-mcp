{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Options (
  Outcome (..),
  parseOptions,
) where

import Agda.Interaction.Library (parseLibName)
import Agda.Interaction.Options (
  CommandLineOptions (..),
  defaultOptions,
 )
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative (
  Parser,
  ParserInfo,
  ParserResult (CompletionInvoked, Failure, Success),
  defaultPrefs,
  execParserPure,
  flag',
  fullDesc,
  header,
  help,
  info,
  long,
  many,
  metavar,
  renderFailure,
  short,
  strOption,
 )
import Options.Applicative.Extra (helperWith)
import System.Exit (ExitCode (ExitSuccess))

data Outcome
  = Parsed CommandLineOptions
  | Helped Text
  | Rejected Text

parseOptions :: [String] -> Outcome
parseOptions arguments =
  case execParserPure defaultPrefs information arguments of
    Success flags -> Parsed $ foldl (flip ($)) defaultOptions flags
    Failure failure -> case renderFailure failure programName of
      (text, ExitSuccess) -> Helped $ trimmed text
      (text, _) -> Rejected $ trimmed text
    CompletionInvoked _ ->
      Rejected "agda-mcp provides no shell completion."
 where
  trimmed = Text.strip . Text.pack

programName :: String
programName = "agda-mcp"

information :: ParserInfo [CommandLineOptions -> CommandLineOptions]
information =
  info
    (helperWith helpFlag <*> descriptions)
    (fullDesc <> header "agda-mcp")
 where
  helpFlag = long "help" <> short 'h' <> help "show this help text"

descriptions :: Parser [CommandLineOptions -> CommandLineOptions]
descriptions =
  many $
    asum
      [ includeFlag
          <$> strOption
            ( short 'i'
                <> long "include-path"
                <> metavar "DIR"
                <> help "look for imports in DIR"
            )
      , libraryFlag
          <$> strOption
            ( short 'l'
                <> long "library"
                <> metavar "LIB"
                <> help "use library LIB"
            )
      , overrideLibrariesFileFlag
          <$> strOption
            ( long "library-file"
                <> metavar "FILE"
                <> help "use FILE instead of the standard libraries file"
            )
      , flag'
          noLibsFlag
          ( long "no-libraries"
              <> help "don't use any library files"
          )
      , flag'
          noDefaultLibsFlag
          ( long "no-default-libraries"
              <> help "don't use default libraries"
          )
      ]
 where
  includeFlag directory options =
    options {optIncludePaths = directory : optIncludePaths options}
  libraryFlag name options =
    options {optLibraries = optLibraries options <> [parseLibName name]}
  overrideLibrariesFileFlag file options =
    options {optOverrideLibrariesFile = Just file, optUseLibs = True}
  noLibsFlag options = options {optUseLibs = False}
  noDefaultLibsFlag options = options {optDefaultLibs = False}
