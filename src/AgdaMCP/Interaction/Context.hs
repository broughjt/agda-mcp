module AgdaMCP.Interaction.Context (Request (..)) where

import Agda.Interaction.Base (Rewrite)
import Agda.Interaction.Command (CommandM)
import Agda.Syntax.Common (InteractionId)

import AgdaMCP.Interaction.Internal (InteractionM, runCommandM)
import AgdaMCP.Interaction.Model (ContextEntry, Error)

-- import AgdaMCP.Interaction.Model (Error)

data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  }

type Response = Either Error ContextEntry

context :: Request -> InteractionM Response
context = runCommandM . contextInternal

contextInternal :: Request -> CommandM Response
contextInternal = error "un"
