{-# LANGUAGE OverloadedStrings #-}

{- | The client side of the daemon: one connection per request, and the
connect protocol that starts or replaces a daemon on the way.
-}
module AgdaMCP.Interact.Daemon.Client (
  Plumbing (..),
  openConnection,
  hello,
  request,
  control,
  withDaemon,
) where

import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import Network.Socket (
  Family (AF_UNIX),
  SockAddr (SockAddrUnix),
  SocketType (Stream),
  connect,
  defaultProtocol,
  socket,
  socketToHandle,
 )
import System.IO (Handle, IOMode (ReadWriteMode))

import AgdaMCP.Interact.Daemon.Key (Key, Paths (..))
import AgdaMCP.Interact.Daemon.Protocol (
  BuildIdentity,
  Control,
  HelloRequest (..),
  HelloResponse,
  Request,
  Response,
  receive,
  send,
 )

{- | The daemon could not be reached or did not answer: nothing was answered,
so the client reports the text and exits 2.
-}
newtype Plumbing = Plumbing Text
  deriving (Show)

instance Exception Plumbing

{- | Connect to the daemon's socket, without starting one. The handle is the
connection: every line of the exchange goes through this one, since a second
handle on the same socket would buffer separately and could consume the
other's input.
-}
openConnection :: Paths -> IO Handle
openConnection paths = do
  connection <- socket AF_UNIX Stream defaultProtocol
  connect connection $ SockAddrUnix $ socketPath paths
  socketToHandle connection ReadWriteMode

{- | Send the hello and read the reply. @Nothing@ means the daemon closed
before replying and the request never started, so the caller may retry once.
-}
hello :: Handle -> BuildIdentity -> IO (Maybe HelloResponse)
hello connection build = do
  send connection $ HelloBuild build
  receive connection

-- | Send one request after a successful hello and read its response.
request :: Handle -> Request -> IO Response
request connection body = do
  send connection body
  receive connection >>= maybe (throwIO $ Plumbing exited) pure

-- | Send a control line in place of the hello and read its response.
control :: Handle -> Control -> IO Response
control connection command = do
  send connection $ HelloControl command
  receive connection >>= maybe (throwIO $ Plumbing exited) pure

exited :: Text
exited = "agda-interact daemon exited during the request"

{- | Run an action against the daemon for the key, starting one if none is
listening and replacing one whose build differs from this process's. Takes an
idle timeout in seconds for the daemon spawn.
-}
withDaemon :: Key -> Int -> (Handle -> IO a) -> IO a
withDaemon = error "not implemented"
