{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Options (
  AgdaFlags (..),
  defaultFlags,
  flagsParser,
  toCommandLineOptions,
  renderFlags,
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

{- | The Agda command-line flags a session is started under, as parsed, before
they are folded into Agda's own `CommandLineOptions`. This is the form the
daemon key hashes and the spawn passes on, since it can be rendered back to
argv; `CommandLineOptions` is only ever built from it inside the process that
will construct a `ToolState`.
-}
data AgdaFlags = AgdaFlags
  { includes :: [FilePath]
  -- ^ Each `-i DIR`, in argv order.
  , libraries :: [String]
  -- ^ Each `-l LIB`, in argv order, as written.
  , libraryFile :: Maybe FilePath
  -- ^ `--library-file FILE`.
  , useLibs :: Bool
  -- ^ False when `--no-libraries` was given.
  , defaultLibs :: Bool
  -- ^ False when `--no-default-libraries` was given.
  }
  deriving (Eq, Show)

defaultFlags :: AgdaFlags
defaultFlags =
  AgdaFlags
    { includes = []
    , libraries = []
    , libraryFile = Nothing
    , useLibs = True
    , defaultLibs = True
    }

flagsParser :: Parser AgdaFlags
flagsParser = error "not implemented"

{- | Fold the flags into Agda's options exactly as the command line always has
been: `-i` prepends, so `optIncludePaths` ends up in reverse argv order, and
`-l` appends.
-}
toCommandLineOptions :: AgdaFlags -> CommandLineOptions
toCommandLineOptions = error "not implemented"

-- | The flags as argv, in canonical order, using the long spellings.
renderFlags :: AgdaFlags -> [String]
renderFlags = error "not implemented"

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
