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
import Data.Bifunctor (bimap)
import Data.Text (Text)

import AgdaMCP.Interaction.GiveFamily (giveGen', withResolvedHole)
import AgdaMCP.Interaction.Internal (
  InteractionM,
  runCommandMWithLoadedFile,
 )
import AgdaMCP.Interaction.Model (
  GiveAction (..),
  GiveError (..),
  NoLoadedFile,
  Span,
 )

data Request = Request
  { requestForce :: UseForce
  , requestGoalId :: InteractionId
  , requestExpression :: Text
  }

type Response = Either GiveError (Span, GiveAction)

give :: Request -> InteractionM (Either NoLoadedFile Response)
give = runCommandMWithLoadedFile . giveInternal

giveInternal :: Request -> CommandM Response
giveInternal (Request force goalId expression) =
  withResolvedHole GiveUnknownId goalId $ \range holeSpan ->
    bimap (GiveFailed holeSpan) (holeSpan,)
      <$> giveGen' force Give goalId range expression
