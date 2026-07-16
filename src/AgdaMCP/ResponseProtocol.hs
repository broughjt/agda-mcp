module AgdaMCP.ResponseProtocol (
  AgdaResponseMismatch (..),
  throwMismatch,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)

import Agda.Interaction.JSON (EncodeTCM (encodeTCM))
import Agda.Interaction.JSONTop ()
import Agda.Interaction.Response (Response)

import Agda.TypeChecking.Monad (TCM)

-- A list of responses emitted by Agda did not match our mental model for the
-- pattern of possible responses.
data AgdaResponseMismatch a = AgdaResponseMismatch
  { mismatchCommand :: String
  , mismatchResponses :: [a]
  }
  deriving (Foldable, Functor, Show, Traversable)

instance Exception (AgdaResponseMismatch Value)

--- An `AgdaResponseMismatch` is always a bug in agda-mcp. If we encounter one,
--- we encode the raw responses to JSON while their type-checking state is
--- available in the type-checking monad and then die loudly with the resulting
--- debugging information.
throwMismatch :: AgdaResponseMismatch Response -> TCM a
throwMismatch mismatch = traverse encodeTCM mismatch >>= liftIO . throwIO
