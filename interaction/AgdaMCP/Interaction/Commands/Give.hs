module AgdaMCP.Interaction.Commands.Give (
  Request (..),
  Response,
  give,
  giveInternal,
) where

import Agda.Interaction.Base (UseForce (..))
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (GiveRefine (..))
import Agda.Syntax.Common (InteractionId)
import Control.Monad.State (get)
import Data.Bifunctor (bimap)
import Data.Text (Text)

import AgdaMCP.Interaction.GiveFamily (giveGen', withResolvedHole)
import AgdaMCP.Interaction.Internal (
  GiveSlot,
  InteractionM,
  InteractionState (..),
  runCommandM,
 )
import AgdaMCP.Interaction.Model (
  GiveAction (..),
  GiveError (..),
  Span,
 )

data Request = Request
  { requestForce :: UseForce
  , requestGoalId :: InteractionId
  , requestExpression :: Text
  }

type Response = Either GiveError (Span, GiveAction)

give :: Request -> InteractionM Response
give request = do
  InteractionState _ _ slot <- get
  runCommandM (giveInternal slot request)

giveInternal :: GiveSlot -> Request -> CommandM Response
giveInternal slot (Request force goalId expression) =
  withResolvedHole GiveUnknownId goalId $ \range holeSpan ->
    bimap (GiveFailed holeSpan) (holeSpan,)
      <$> giveGen' slot force Give goalId range expression
