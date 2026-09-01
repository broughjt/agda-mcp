{-# LANGUAGE OverloadedStrings #-}

{- | The wire protocol between client and daemon. Newline-delimited JSON over
a Unix socket, with one request per connection. The first line each way is the
hello, which never changes shape so that a mismatched build can always be
recognized and stopped. A control line may stand in for it.
-}
module AgdaMCP.Interact.Daemon.Protocol (
  BuildIdentity (..),
  currentBuild,
  Control (..),
  HelloRequest (..),
  HelloResponse (..),
  Request (..),
  Response (..),
  Info (..),
  send,
  receive,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import System.IO (Handle)

{- | Which build of the program a process is. The path alone is not enough:
cabal rewrites the same path with a new modification time, and Nix gives a new
path with the modification time pinned to the epoch.
-}
data BuildIdentity = BuildIdentity
  { buildExecutable :: FilePath
  , buildSize :: Integer
  , buildModified :: Integer
  -- ^ Modification time as POSIX seconds.
  }
  deriving (Eq, Show)

-- | This process's build, from its own executable.
currentBuild :: IO BuildIdentity
currentBuild = error "not implemented"

data Control = ControlStop | ControlStatus
  deriving (Eq, Show)

-- | The client's first line.
data HelloRequest
  = HelloBuild BuildIdentity
  | {- | Honoured regardless of build, so a mismatched client can stop the
    daemon it is about to replace.
    -}
    HelloControl Control
  deriving (Eq, Show)

-- | The daemon's response to `HelloRequest`. After a mismatch it closes.
data HelloResponse
  = BuildOk
  | BuildMismatch BuildIdentity
  deriving (Eq, Show)

{- | A tool call, consisting of the tool name and the arguments the MCP layer
would get.
-}
data Request = Request
  { requestTool :: Text
  , requestArguments :: Map Text Value
  }
  deriving (Eq, Show)

{- | The answer to a request or a control: whether it succeeded, the text the
tool rendered with no trailing newline, and the same answer as JSON.
-}
data Response = Response
  { responseOk :: Bool
  , responseText :: Text
  , responseValue :: Value
  }
  deriving (Eq, Show)

{- | Information a daemon records about itself at start, for @status@ and for
@ls@.
-}
data Info = Info
  { infoRoot :: FilePath
  , infoFlags :: [String]
  , infoPid :: Int
  , infoBuild :: BuildIdentity
  , infoAgdaVersion :: Text
  , infoStarted :: UTCTime
  }
  deriving (Eq, Show)

instance ToJSON BuildIdentity where
  toJSON = error "not implemented"

instance FromJSON BuildIdentity where
  parseJSON = error "not implemented"

instance ToJSON HelloRequest where
  toJSON = error "not implemented"

instance FromJSON HelloRequest where
  parseJSON = error "not implemented"

instance ToJSON HelloResponse where
  toJSON = error "not implemented"

instance FromJSON HelloResponse where
  parseJSON = error "not implemented"

instance ToJSON Request where
  toJSON = error "not implemented"

instance FromJSON Request where
  parseJSON = error "not implemented"

instance ToJSON Response where
  toJSON = error "not implemented"

instance FromJSON Response where
  parseJSON = error "not implemented"

instance ToJSON Info where
  toJSON = error "not implemented"

instance FromJSON Info where
  parseJSON = error "not implemented"

-- | Write one value as one line.
send :: (ToJSON a) => Handle -> a -> IO ()
send = error "not implemented"

{- | Read one line and decode it. @Nothing@ means the peer closed before
sending a line. A line that fails to decode throws.
-}
receive :: (FromJSON a) => Handle -> IO (Maybe a)
receive = error "not implemented"
