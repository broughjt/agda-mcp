module AgdaMCP.Interaction.Refine (
  Request (..),
  Response,
  refine,
  refineInternal,
) where

import Agda.Interaction.Base (UseForce (..))
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (GiveRefine (..))
import Agda.Syntax.Common (InteractionId)
import Control.Monad.State (get)
import Data.Text (Text)

import AgdaMCP.Interaction.Give (Response, giveGen')
import AgdaMCP.Interaction.Internal (
  GiveSlot,
  InteractionM,
  InteractionState (..),
  runCommandM,
 )

-- Refine always sets the force parameter to false, so we do not include it
-- here.
data Request = Request
  { requestGoalId :: InteractionId
  , requestExpression :: Text
  }

refine :: Request -> InteractionM Response
refine request = do
  InteractionState _ _ slot <- get
  runCommandM $ refineInternal slot request

refineInternal :: GiveSlot -> Request -> CommandM Response
refineInternal slot (Request goalId expression) =
  giveGen' slot WithoutForce Refine goalId expression
