{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Interact.CLI (
  Mode (..),
  Format (..),
  Invocation (..),
  defaultIdleTimeout,
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

import AgdaMCP.Interact.Daemon.Protocol (Response)
import AgdaMCP.Options (AgdaFlags)

data Mode
  = -- | The @daemon@ verb: serve the socket in the foreground.
    RunDaemon
  | {- | A tool verb: the tool's name and the arguments it would receive
    over MCP.
    -}
    Invoke Text (Map Text Value)
  | Stop
  | Status
  deriving (Eq, Show)

-- | How the client prints an answer. Either prose, or @{"ok": B, "value": V}@.
data Format = FormatText | FormatJson
  deriving (Eq, Show)

-- | Seconds without a request after which the daemon exits.
defaultIdleTimeout :: Int
defaultIdleTimeout = 2 * 60 * 60 -- 2 hours

data Invocation = Invocation
  { invocationFlags :: AgdaFlags
  , invocationIdleTimeout :: Int
  -- ^ Seconds; honoured by @daemon@ and forwarded to a spawn by other verbs.
  , invocationFormat :: Format
  , invocationMode :: Mode
  }
  deriving (Eq, Show)

data Outcome
  = Parsed Invocation
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

{- | The daemon answered. As text, a refusal or error goes to stderr with exit
1. As JSON, the envelope goes to stdout either way, with the same exit code.
-}
answered :: Format -> Response -> Exit
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
