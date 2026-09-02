{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Options (
  AgdaFlags (..),
  defaultFlags,
  flagsParser,
  toCommandLineOptions,
  renderFlags,
) where

import Agda.Interaction.Library (parseLibName)
import Agda.Interaction.Options (CommandLineOptions (..), defaultOptions)
import Control.Applicative ((<|>))
import Options.Applicative (
  Parser,
  flag,
  help,
  long,
  many,
  metavar,
  optional,
  short,
  strOption,
 )

{- | The Agda command-line flags a session is started under, as parsed, before
they are folded into Agda's `CommandLineOptions`. This is the form the daemon
key hashes and the spawn passes on, since it can be rendered back to argv;
`CommandLineOptions` is only ever built from it inside the process that will
construct a `ToolState`.
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
flagsParser =
  AgdaFlags
    <$> many
      ( strOption
          ( short 'i'
              <> long "include-path"
              <> metavar "DIR"
              <> help "look for imports in DIR"
          )
      )
    <*> many
      ( strOption
          ( short 'l'
              <> long "library"
              <> metavar "LIB"
              <> help "use library LIB"
          )
      )
    <*> optional
      ( strOption
          ( long "library-file"
              <> metavar "FILE"
              <> help "use FILE instead of the standard libraries file"
          )
      )
    <*> flag
      True
      False
      ( long "no-libraries"
          <> help "don't use any library files"
      )
    <*> flag
      True
      False
      ( long "no-default-libraries"
          <> help "don't use default libraries"
      )

{- | Fold the flags into Agda's options: `-i` prepends, so `optIncludePaths`
ends up in reverse argv order, and `-l` appends.
-}
toCommandLineOptions :: AgdaFlags -> CommandLineOptions
toCommandLineOptions flags =
  defaultOptions
    { optIncludePaths =
        foldl (flip (:)) (optIncludePaths defaultOptions) (includes flags)
    , optLibraries =
        optLibraries defaultOptions <> map parseLibName (libraries flags)
    , optOverrideLibrariesFile =
        libraryFile flags <|> optOverrideLibrariesFile defaultOptions
    , optUseLibs = useLibs flags
    , optDefaultLibs = defaultLibs flags
    }

-- | The flags as argv, in canonical order, using the long spellings.
renderFlags :: AgdaFlags -> [String]
renderFlags flags =
  concatMap (\directory -> ["--include-path", directory]) (includes flags)
    <> concatMap (\name -> ["--library", name]) (libraries flags)
    <> maybe [] (\file -> ["--library-file", file]) (libraryFile flags)
    <> ["--no-libraries" | not (useLibs flags)]
    <> ["--no-default-libraries" | not (defaultLibs flags)]
