{-# LANGUAGE OverloadedStrings #-}

{- | The command-line grammar: Agda flags, then a lifecycle flag, then a verb.
Bare @agda-mcp@ is the MCP server on stdio, as it always was; each tool verb
assembles exactly the argument object the MCP layer would have received, so
the tool modules' request parsers are reused unchanged.
-}
module AgdaMCP.CLI (
  Mode (..),
  Lifecycle (..),
  defaultLifecycle,
  Outcome (..),
  parseCommandLine,
  Stream (..),
  Exit (..),
  answered,
  helped,
  rejected,
  plumbing,
) where

import Data.Aeson (Value)
import Data.Map (Map)
import Data.Text (Text)
import System.Exit (ExitCode)

import AgdaMCP.Daemon.Protocol (Response)
import AgdaMCP.Tools.Options (AgdaFlags)

data Mode
  = -- | No verb: run the MCP server on stdio.
    Serve
  | -- | The @daemon@ verb: serve the socket in the foreground.
    RunDaemon
  | {- | A tool verb: the tool's name and the arguments it would receive
    over MCP.
    -}
    Invoke Text (Map Text Value)
  | Stop
  | Status
  deriving (Eq, Show)

newtype Lifecycle = Lifecycle
  { idleTimeout :: Int
  -- ^ Seconds without a request after which the daemon exits.
  }
  deriving (Eq, Show)

defaultLifecycle :: Lifecycle
defaultLifecycle = Lifecycle {idleTimeout = 2 * 60 * 60}

data Outcome
  = Parsed AgdaFlags Lifecycle Mode
  | Helped Text
  | Rejected Text
  deriving (Eq, Show)

{- | Parse argv. The directory is the client's working directory, against which
@load@'s path is made absolute, since the daemon's is not the client's.
-}
parseCommandLine :: FilePath -> [String] -> Outcome
parseCommandLine = error "not implemented"

-- Exit mapping

data Stream = Stdout | Stderr
  deriving (Eq, Show)

-- | How a client invocation ends: what it prints, where, and the exit code.
data Exit = Exit
  { exitText :: Text
  , exitStream :: Stream
  , exitCode :: ExitCode
  }
  deriving (Eq, Show)

-- | The daemon answered. A refusal or error goes to stderr with exit 1.
answered :: Response -> Exit
answered = error "not implemented"

-- | Help text: stdout, exit 0.
helped :: Text -> Exit
helped = error "not implemented"

-- | Argv rejected by the parser: stderr, exit 1.
rejected :: Text -> Exit
rejected = error "not implemented"

{- | Plumbing failed before the daemon answered: cannot connect or spawn, not
ready, replace failed, EOF after the hello, malformed response. stderr, exit 2.
-}
plumbing :: Text -> Exit
plumbing = error "not implemented"
