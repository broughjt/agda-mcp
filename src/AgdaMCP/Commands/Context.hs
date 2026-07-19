module AgdaMCP.Commands.Context (Request (..), Response (..)) where

import Agda.Interaction.Base (Rewrite)
import Agda.Syntax.Common (InteractionId)

data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  }

-- The execution paths
data Response = Response {}
