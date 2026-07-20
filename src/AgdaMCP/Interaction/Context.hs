module AgdaMCP.Interaction.Context (Request (..)) where

import Agda.Interaction.Base (Rewrite)
import Agda.Syntax.Common (InteractionId)

-- import AgdaMCP.Interaction.Model (Error)

data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  }

-- type Response = Either Error ContextEntry
