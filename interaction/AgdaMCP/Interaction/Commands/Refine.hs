module AgdaMCP.Interaction.Commands.Refine (
  Request (..),
  Response,
  refine,
  refineInternal,
) where

import Agda.Interaction.Base (UseForce (..))
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (GiveRefine (..))
import Agda.Syntax.Common (InteractionId)
import Data.Bifunctor (bimap)
import Data.Text (Text)

import AgdaMCP.Interaction.Commands.Give (Response)
import AgdaMCP.Interaction.GiveFamily (giveGen', withResolvedHole)
import AgdaMCP.Interaction.Internal (
  InteractionM,
  runCommandM,
 )
import AgdaMCP.Interaction.Model (GiveError (..))

{- | Refine always sets the force parameter to false, so we do not include it
here.
-}
data Request = Request
  { requestGoalId :: InteractionId
  , requestExpression :: Text
  }

refine :: Request -> InteractionM Response
refine = runCommandM . refineInternal

refineInternal :: Request -> CommandM Response
refineInternal (Request goalId expression) =
  withResolvedHole GiveUnknownId goalId $ \range holeSpan ->
    bimap (GiveFailed holeSpan) (holeSpan,)
      <$> giveGen' WithoutForce Refine goalId range expression
