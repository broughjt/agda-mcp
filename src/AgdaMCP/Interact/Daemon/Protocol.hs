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
  MalformedLine (..),
  send,
  receive,
) where

import Control.Exception (Exception, throwIO)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  Value,
  eitherDecodeStrict,
  encode,
  object,
  withObject,
  withText,
  (.:),
  (.=),
 )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Directory (getFileSize, getModificationTime)
import System.Environment (getExecutablePath)
import System.IO (Handle, hFlush, hIsEOF)

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
currentBuild = do
  executable <- getExecutablePath
  size <- getFileSize executable
  modified <- getModificationTime executable
  pure
    BuildIdentity
      { buildExecutable = executable
      , buildSize = size
      , buildModified = floor $ utcTimeToPOSIXSeconds modified
      }

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
  toJSON build =
    object
      [ "executable" .= buildExecutable build
      , "size" .= buildSize build
      , "modified" .= buildModified build
      ]

instance FromJSON BuildIdentity where
  parseJSON = withObject "build" $ \o ->
    BuildIdentity
      <$> o .: "executable"
      <*> o .: "size"
      <*> o .: "modified"

controlName :: Control -> Text
controlName ControlStop = "stop"
controlName ControlStatus = "status"

parseControl :: Value -> Aeson.Parser Control
parseControl = withText "control" $ \name ->
  case name of
    "stop" -> pure ControlStop
    "status" -> pure ControlStatus
    _ -> fail $ "expected stop or status, but got " <> show name

instance ToJSON HelloRequest where
  toJSON (HelloBuild build) = object ["build" .= build]
  toJSON (HelloControl control) = object ["control" .= controlName control]

instance FromJSON HelloRequest where
  parseJSON = withObject "hello" $ \o ->
    case (KeyMap.lookup "build" o, KeyMap.lookup "control" o) of
      (Just build, _) -> HelloBuild <$> parseJSON build
      (_, Just control) -> HelloControl <$> parseControl control
      _ -> fail "expected a build or a control"

instance ToJSON HelloResponse where
  toJSON BuildOk = object ["build" .= ("ok" :: Text)]
  toJSON (BuildMismatch build) =
    object ["build" .= ("mismatch" :: Text), "daemon" .= build]

instance FromJSON HelloResponse where
  parseJSON = withObject "hello response" $ \o -> do
    verdict <- o .: "build"
    case verdict :: Text of
      "ok" -> pure BuildOk
      "mismatch" -> BuildMismatch <$> o .: "daemon"
      _ -> fail $ "expected ok or mismatch, but got " <> show verdict

instance ToJSON Request where
  toJSON request =
    object
      [ "tool" .= requestTool request
      , "arguments" .= requestArguments request
      ]

instance FromJSON Request where
  parseJSON = withObject "request" $ \o ->
    Request <$> o .: "tool" <*> o .: "arguments"

instance ToJSON Response where
  toJSON response =
    object
      [ "ok" .= responseOk response
      , "text" .= responseText response
      , "value" .= responseValue response
      ]

instance FromJSON Response where
  parseJSON = withObject "response" $ \o ->
    Response <$> o .: "ok" <*> o .: "text" <*> o .: "value"

instance ToJSON Info where
  toJSON info =
    object
      [ "root" .= infoRoot info
      , "flags" .= infoFlags info
      , "pid" .= infoPid info
      , "build" .= infoBuild info
      , "agda_version" .= infoAgdaVersion info
      , "started" .= infoStarted info
      ]

instance FromJSON Info where
  parseJSON = withObject "info" $ \o ->
    Info
      <$> o .: "root"
      <*> o .: "flags"
      <*> o .: "pid"
      <*> o .: "build"
      <*> o .: "agda_version"
      <*> o .: "started"

-- | A line that arrived but did not decode as the value expected.
newtype MalformedLine = MalformedLine String
  deriving (Show)

instance Exception MalformedLine

{- | Write one value as one line. Framing is sound because the encoder escapes
control characters, so the value never contains a raw newline.
-}
send :: (ToJSON a) => Handle -> a -> IO ()
send handle value = do
  ByteString.Lazy.hPut handle $ encode value <> "\n"
  hFlush handle

{- | Read one line and decode it. @Nothing@ means the peer closed before
sending a line. A line that fails to decode throws.
-}
receive :: (FromJSON a) => Handle -> IO (Maybe a)
receive handle = do
  closed <- hIsEOF handle
  if closed
    then pure Nothing
    else do
      line <- ByteString.hGetLine handle
      either (throwIO . MalformedLine) (pure . Just) $ eitherDecodeStrict line
