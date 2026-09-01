{- | The daemon. An Agda session behind a Unix socket which dispatches requests
over the same tool table the MCP server uses.
-}
module AgdaMCP.Interact.Daemon.Server (
  Daemon (..),
  serve,
) where

import Control.Concurrent.MVar (MVar)

import AgdaMCP.Interact.Daemon.Key (Key, Paths)
import AgdaMCP.Interact.Daemon.Protocol (BuildIdentity)
import AgdaMCP.Tools.State (ToolState)

data Daemon = Daemon
  { daemonKey :: Key
  , daemonPaths :: Paths
  , daemonBuild :: BuildIdentity
  , daemonIdleTimeout :: Int
  -- ^ Seconds without a request after which the daemon exits.
  , daemonReady :: IO ()
  {- ^ Run once the socket is listening and the info file is written. The
  executable prints @ready@ to the pipe its parent is waiting on.
  -}
  }

{- | Serve until told to stop or left idle. In order: take the daemon lock
(exit if another daemon holds it), unlink a stale socket, bind and listen,
write the info file, signal readiness, then loop. Each connection runs in its
own thread. Shutdown closes the listener and unlinks the socket first, then
waits for in-flight connections, then unlinks the info file.
-}
serve :: Daemon -> MVar ToolState -> IO ()
serve = error "not implemented"
