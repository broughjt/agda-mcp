{-# LANGUAGE OverloadedStrings #-}

{- | The wire protocol between client and daemon: newline-delimited JSON over
a Unix socket, one request per connection. The first line each way is the
hello, which never changes shape so that a mismatched build can always be
recognised and stopped; a control line may stand in for it.
-}
module AgdaMCP.Daemon.Protocol (
  BuildIdentity (..),
  currentBuild,
  Control (..),
  Hello (..),
  HelloReply (..),
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
import Network.Socket (Socket)

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
data Hello
  = HelloBuild BuildIdentity
  | {- | Honoured regardless of build, so a mismatched client can stop the
    daemon it is about to replace.
    -}
    HelloControl Control
  deriving (Eq, Show)

-- | The daemon's reply to `HelloBuild`. After a mismatch it closes.
data HelloReply
  = BuildOk
  | BuildMismatch BuildIdentity
  deriving (Eq, Show)

-- | A tool call: the tool's name and the arguments the MCP layer would get.
data Request = Request
  { requestTool :: Text
  , requestArguments :: Map Text Value
  }
  deriving (Eq, Show)

{- | The answer to a request or a control: whether it succeeded, and the text
the tool rendered, with no trailing newline.
-}
data Response = Response
  { responseOk :: Bool
  , responseText :: Text
  }
  deriving (Eq, Show)

-- | What a daemon records about itself at start, for @status@ and for @ls@.
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

instance ToJSON Hello where
  toJSON = error "not implemented"

instance FromJSON Hello where
  parseJSON = error "not implemented"

instance ToJSON HelloReply where
  toJSON = error "not implemented"

instance FromJSON HelloReply where
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
send :: (ToJSON a) => Socket -> a -> IO ()
send = error "not implemented"

{- | Read one line and decode it. @Nothing@ means the peer closed before
sending a line; a line that does not decode throws.
-}
receive :: (FromJSON a) => Socket -> IO (Maybe a)
receive = error "not implemented"
