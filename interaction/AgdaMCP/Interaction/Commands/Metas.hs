module AgdaMCP.Interaction.Commands.Metas (
  Request (..),
  Response,
  metas,
) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Interaction.Command (CommandM)
import Control.Monad.State (lift)

import AgdaMCP.Interaction.Extract (extractError, extractMetas)
import AgdaMCP.Interaction.Internal (
  InteractionM,
  catchTCErr,
  runCommandMWithLoadedFile,
 )
import AgdaMCP.Interaction.Model (
  Error,
  MetasReport (..),
  NoLoadedFile,
 )

-- | @Cmd_metas@ takes the degree of normalization to render goal types at.
data Request = Request {requestNormalization :: Rewrite}
  deriving (Eq, Show)

type Response = Either Error MetasReport

metas :: Request -> InteractionM (Either NoLoadedFile Response)
metas = runCommandMWithLoadedFile . metasInternal

metasInternal :: Request -> CommandM Response
metasInternal (Request normalization) =
  (Right <$> lift (extractMetas normalization))
    `catchTCErr` (fmap Left . lift . extractError)
