module AgdaMCP.Interaction.ElaborateGive (
  Request (..),
  Response,
  elaborateGive,
  elaborateGiveInternal,
) where

import Agda.Interaction.Base (Rewrite, UseForce (..))
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

-- Elaborate-give always sets the force parameter to false, so we do not include
-- that in the request. It does carry a normalization mode for the elaborated
-- term. The result is always a computed splice (`Give_String`).
data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  , requestExpression :: Text
  }

elaborateGive :: Request -> InteractionM Response
elaborateGive request = do
  InteractionState _ _ slot <- get
  runCommandM $ elaborateGiveInternal slot request

elaborateGiveInternal :: GiveSlot -> Request -> CommandM Response
elaborateGiveInternal slot (Request norm goalId expression) =
  giveGen' slot WithoutForce (ElaborateGive norm) goalId expression
