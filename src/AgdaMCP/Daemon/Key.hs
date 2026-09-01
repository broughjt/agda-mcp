{-# LANGUAGE OverloadedStrings #-}

{- | What identifies a daemon: the project root the client is working in and
the Agda flags its session runs under. Everything a client needs to find,
start or stop a daemon derives from the key.
-}
module AgdaMCP.Daemon.Key (
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

import AgdaMCP.Tools.Options (AgdaFlags)

data Key = Key
  { keyRoot :: FilePath
  {- ^ Absolute project root: the nearest ancestor of the working directory
  holding an @.agda-lib@ file, or the working directory itself.
  -}
  , keyFlags :: AgdaFlags
  {- ^ With @--library-file@ made absolute, since only it is read relative to
  the working directory.
  -}
  }
  deriving (Eq, Show)

{- | The project root for a working directory, found the way Agda's
@findProjectConfig@ finds it: walk up from the canonicalised directory to the
first one containing a file with the @.agda-lib@ extension. Reaching the
filesystem root, or a directory that cannot be read, answers the working
directory itself.
-}
findProjectRoot :: FilePath -> IO FilePath
findProjectRoot = error "not implemented"

-- | The key for a working directory and the flags as given on its command line.
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

{- | Where daemons keep their sockets: @$XDG_RUNTIME_DIR/agda-mcp@, or
@/tmp/agda-mcp-<uid>@ created with mode 0700 when that variable is unset. The
directory exists when this returns.
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
