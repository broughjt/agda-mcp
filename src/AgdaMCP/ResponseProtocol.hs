module AgdaMCP.ResponseProtocol (
  AgdaResponseMismatch (..),
  fromProtocolResult,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)

import Agda.Interaction.JSON (EncodeTCM (encodeTCM))
import Agda.Interaction.JSONTop ()
import Agda.Interaction.Response (Response)

import AgdaMCP.Session (SessionM, liftTCM)

-- A list of responses emitted by Agda did not match our mental model for the
-- pattern of possible responses.
data AgdaResponseMismatch a = AgdaResponseMismatch
  { mismatchCommand :: String
  , mismatchResponses :: [a]
  }
  deriving (Foldable, Functor, Show, Traversable)

instance Exception (AgdaResponseMismatch Value)

-- An `AgdaResponseMismatch` is always a bug in agda-mcp. If we encounter one,
-- we encode the raw responses while their type-checking state is available and
-- then die loudly with the resulting debugging information.
fromProtocolResult ::
  Either (AgdaResponseMismatch Response) a -> SessionM a
fromProtocolResult = either throwMismatch pure
 where
  throwMismatch mismatch =
    liftTCM (traverse encodeTCM mismatch) >>= liftIO . throwIO
