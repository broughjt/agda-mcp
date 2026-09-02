{-# LANGUAGE OverloadedStrings #-}

{- | Daemon identity, consisting of the project root and the session's Agda
flags.
-}
module AgdaMCP.Interact.Daemon.Key (
  Key (..),
  findProjectRoot,
  newKey,
  keyText,
  keyHash,
  runtimeDirectory,
  runtimeDirectoryFor,
  Paths (..),
  keyPaths,
  preparePaths,
) where

import Control.Exception (IOException, try)
import Data.Bits (xor)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory (
  canonicalizePath,
  createDirectoryIfMissing,
  listDirectory,
 )
import System.Environment (lookupEnv)
import System.FilePath (
  isAbsolute,
  takeDirectory,
  takeExtension,
  (</>),
 )
import System.Posix.Files (setFileMode)
import System.Posix.Types (UserID)
import System.Posix.User (getRealUserID)

import AgdaMCP.Options (AgdaFlags (..), renderFlags)

data Key = Key
  { keyRoot :: FilePath
  {- ^ Absolute project root path: the nearest ancestor of the working directory
  holding an @.agda-lib@ file, or the working directory itself.
  -}
  , keyFlags :: AgdaFlags
  {- ^ With @--library-file@ made absolute and canonical, since it is read
  relative to the working directory in contrast to other options.
  -}
  }
  deriving (Eq, Show)

{- | The project root for a working directory, found the way Agda's
@findProjectConfig@ finds it: walk up from the canonicalized directory to the
first one containing a file with the @.agda-lib@ extension. Reaching the
filesystem root, or a directory that cannot be read, answers the working
directory itself.
-}
findProjectRoot :: FilePath -> IO FilePath
findProjectRoot directory = do
  start <- canonicalizePath directory
  let walk candidate = do
        entries <- try $ listDirectory candidate
        case entries of
          Left (_ :: IOException) -> pure start
          Right names
            | any ((== ".agda-lib") . takeExtension) names -> pure candidate
            | parent == candidate -> pure start
            | otherwise -> walk parent
           where
            parent = takeDirectory candidate
  walk start

{- | The key for a given working directory and set of command flags. The
library file is canonicalized, not merely made absolute, so that two spellings
of one file from two working directories address one daemon.
-}
newKey :: FilePath -> AgdaFlags -> IO Key
newKey directory flags = do
  root <- findProjectRoot directory
  library <- traverse (canonicalizePath . absolute) (libraryFile flags)
  pure Key {keyRoot = root, keyFlags = flags {libraryFile = library}}
 where
  absolute path
    | isAbsolute path = path
    | otherwise = directory </> path

{- | The text the key hashes: the root, then each rendered flag, separated by
NUL, which no path or flag can contain.
-}
keyText :: Key -> Text
keyText key =
  Text.pack (keyRoot key)
    <> "\0"
    <> Text.intercalate "\0" (map Text.pack $ renderFlags $ keyFlags key)

{- | FNV-1a 64 over the UTF-8 of `keyText`, as sixteen lowercase hex digits.
Written by hand rather than taken from a library so that two builds of the
program compute the same directory for the same key, or a rebuild would
strand the daemon the old build started.
-}
keyHash :: Key -> Text
keyHash key = Text.justifyRight 16 '0' $ Text.pack $ showHex digest ""
 where
  digest :: Word64
  digest =
    foldl'
      (\hash byte -> (hash `xor` fromIntegral byte) * 0x100000001b3)
      0xcbf29ce484222325
      (ByteString.unpack $ encodeUtf8 $ keyText key)

{- | The daemon socket location: @$XDG_RUNTIME_DIR/agda-interact@, or
@/tmp/agda-interact-<uid>@ with mode 0700 when that variable is unset. The
directory will exist when this returns.
-}
runtimeDirectory :: IO FilePath
runtimeDirectory = do
  runtime <- lookupEnv "XDG_RUNTIME_DIR"
  user <- getRealUserID
  let directory = runtimeDirectoryFor runtime user
  createDirectoryIfMissing True directory
  -- The runtime directory is the user's own; the fallback under /tmp is
  -- shared, so it is closed to everyone else.
  setFileMode directory 0o700
  pure directory

-- | The directory `runtimeDirectory` chooses, given @XDG_RUNTIME_DIR@'s value.
runtimeDirectoryFor :: Maybe FilePath -> UserID -> FilePath
runtimeDirectoryFor runtime user =
  case runtime of
    Just directory | not (null directory) -> directory </> "agda-interact"
    _ -> "/tmp/agda-interact-" <> show user

-- | The files of one daemon, all under @<runtime>/<hash>/@.
data Paths = Paths
  { socketPath :: FilePath
  , infoPath :: FilePath
  , logPath :: FilePath
  , spawnLockPath :: FilePath
  , daemonLockPath :: FilePath
  }
  deriving (Eq, Show)

keyPaths :: FilePath -> Key -> Paths
keyPaths runtime key =
  Paths
    { socketPath = directory </> "socket"
    , infoPath = directory </> "info"
    , logPath = directory </> "log"
    , spawnLockPath = directory </> "spawn.lock"
    , daemonLockPath = directory </> "daemon.lock"
    }
 where
  directory = runtime </> Text.unpack (keyHash key)

-- | The key's paths under this user's runtime directory, with their directory created.
preparePaths :: Key -> IO Paths
preparePaths key = do
  runtime <- runtimeDirectory
  createDirectoryIfMissing True $ runtime </> Text.unpack (keyHash key)
  pure $ keyPaths runtime key
