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
) where

import Data.Text (Text)
import System.Posix.Types (UserID)

import AgdaMCP.Options (AgdaFlags)

data Key = Key
  { keyRoot :: FilePath
  {- ^ Absolute project root path: the nearest ancestor of the working directory
  holding an @.agda-lib@ file, or the working directory itself.
  -}
  , keyFlags :: AgdaFlags
  {- ^ With @--library-file@ made absolute, since it is read relative to
  the working directory in contrast to other options.
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
findProjectRoot = error "not implemented"

-- | The key for a given working directory and set of command flags.
newKey :: FilePath -> AgdaFlags -> IO Key
newKey = error "not implemented"

{- | The text the key hashes: the root, then each rendered flag, separated by
NUL, which no path or flag can contain.
-}
keyText :: Key -> Text
keyText = error "not implemented"

{- | FNV-1a 64 over the UTF-8 of `keyText`, as sixteen lowercase hex digits.
Written by hand rather than taken from a library so that two builds of the
program compute the same directory for the same key, or a rebuild would
strand the daemon the old build started.
-}
keyHash :: Key -> Text
keyHash = error "not implemented"

{- | The daemon socket location: @$XDG_RUNTIME_DIR/agda-interact@, or
@/tmp/agda-interact-<uid>@ with mode 0700 when that variable is unset. The
directory will exist when this returns.
-}
runtimeDirectory :: IO FilePath
runtimeDirectory = error "not implemented"

-- | The directory `runtimeDirectory` chooses, given @XDG_RUNTIME_DIR@'s value.
runtimeDirectoryFor :: Maybe FilePath -> UserID -> FilePath
runtimeDirectoryFor = error "not implemented"

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
keyPaths = error "not implemented"
