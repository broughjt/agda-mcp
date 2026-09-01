module AgdaMCP.Interact.Daemon.Client (
  openConnection,
  hello,
  request,
  control,
  withDaemon,
) where

import System.IO (Handle)

import AgdaMCP.Interact.Daemon.Key (Key, Paths)
import AgdaMCP.Interact.Daemon.Protocol (
  BuildIdentity,
  Control,
  HelloResponse,
  Request,
  Response,
 )

{- | Connect to the daemon's socket, without starting one. The handle is the
connection: every line of the exchange goes through this one, since a second
handle on the same socket would buffer separately and could consume the
other's input.
-}
openConnection :: Paths -> IO Handle
openConnection = error "not implemented"

{- | Send the hello and read the reply. @Nothing@ means the daemon closed
before replying and the request never started, so the caller may retry once.
-}
hello :: Handle -> BuildIdentity -> IO (Maybe HelloResponse)
hello = error "not implemented"

-- | Send one request after a successful hello and read its response.
request :: Handle -> Request -> IO Response
request = error "not implemented"

-- | Send a control line in place of the hello and read its response.
control :: Handle -> Control -> IO Response
control = error "not implemented"

{- | Run an action against the daemon for the key, starting one if none is
listening and replacing one whose build differs from this process's. Takes an
idle timeout in seconds for the daemon spawn.
-}
withDaemon :: Key -> Int -> (Handle -> IO a) -> IO a
withDaemon = error "not implemented"
