{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.MCP (
  handlers,
  serverInfo,
  Outcome (..),
  parseOptions,
) where

import Agda.Interaction.Library (parseLibName)
import Agda.Interaction.Options (
  CommandLineOptions (..),
  defaultOptions,
 )
import Control.Concurrent.MVar (MVar)
import Data.Foldable (asum)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  Error (..),
  McpServerHandlers (..),
  McpServerInfo (..),
  ToolDefinition (..),
  noHandlers,
 )
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

import AgdaMCP.Tools (allTools)
import AgdaMCP.Tools.State (ToolState)
import AgdaMCP.Tools.Tool (Tool (..))

serverInfo :: McpServerInfo
serverInfo =
  McpServerInfo
    { serverName = "agda-mcp"
    , serverVersion = "0.1.0.0"
    , serverInstructions = instructions
    }

instructions :: Text
instructions =
  "Tools for developing Agda proofs interactively. Use `load` to type-check a file and see its open goals, inspect them with `goal`, try expressions against them with `check` and `infer`, and fill them with `give` and `case_split`. Agda keeps only one file loaded at a time, and each load reports a load ID that the other tools require. `goal`, `check`, and `infer` do not modify anything, while `give` and `case_split` modify the file on disk and reload it, which issues a new load ID and fresh goal ID assignments."

handlers :: MVar ToolState -> McpServerHandlers
handlers stateVariable =
  noHandlers {tools = Just (list, call)}
 where
  byName :: Map Text Tool
  byName =
    Map.fromList
      [(toolDefinitionName $ toolDefinition tool, tool) | tool <- allTools]

  list _ = pure $ map toolDefinition allTools

  call _ name arguments =
    maybe
      (pure $ Left $ UnknownTool name)
      (\tool -> toolRun tool stateVariable arguments)
      (Map.lookup name byName)

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
