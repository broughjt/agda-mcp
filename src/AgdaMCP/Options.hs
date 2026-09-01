{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Options (
  AgdaFlags (..),
  defaultFlags,
  flagsParser,
  toCommandLineOptions,
  renderFlags,
) where

import Agda.Interaction.Options (CommandLineOptions)
import Options.Applicative (Parser)

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
flagsParser = error "not implemented"

{- | Fold the flags into Agda's options: `-i` prepends, so `optIncludePaths`
ends up in reverse argv order, and `-l` appends.
-}
toCommandLineOptions :: AgdaFlags -> CommandLineOptions
toCommandLineOptions = error "not implemented"

-- | The flags as argv, in canonical order, using the long spellings.
renderFlags :: AgdaFlags -> [String]
renderFlags = error "not implemented"
