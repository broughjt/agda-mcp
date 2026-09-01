{- | The client side: finding or starting the daemon for a key, and one
request over one connection.
-}
module AgdaMCP.Daemon.Client (
  openConnection,
  hello,
  request,
  control,
  withDaemon,
) where

import Network.Socket (Socket)

import AgdaMCP.CLI (Lifecycle)
import AgdaMCP.Daemon.Key (Key, Paths)
import AgdaMCP.Daemon.Protocol (
  BuildIdentity,
  Control,
  HelloReply,
  Request,
  Response,
 )

-- | Connect to the daemon's socket, without starting one.
openConnection :: Paths -> IO Socket
openConnection = error "not implemented"

{- | Send the hello and read the reply. @Nothing@ means the daemon closed
before replying: the request never started, so the caller may retry once.
-}
hello :: Socket -> BuildIdentity -> IO (Maybe HelloReply)
hello = error "not implemented"

-- | Send one request after a successful hello and read its response.
request :: Socket -> Request -> IO Response
request = error "not implemented"

-- | Send a control line in place of the hello and read its response.
control :: Socket -> Control -> IO Response
control = error "not implemented"

{- | Run an action against the daemon for the key, starting one if none is
listening and replacing one whose build differs from this process's.
-}
withDaemon :: Key -> Lifecycle -> (Socket -> IO a) -> IO a
withDaemon = error "not implemented"
