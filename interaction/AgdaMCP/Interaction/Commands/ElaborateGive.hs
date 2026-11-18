module AgdaMCP.Interaction.Commands.ElaborateGive (
  Request (..),
  Response,
  elaborateGive,
  elaborateGiveInternal,
) where

import Agda.Interaction.Base (Rewrite, UseForce (..))
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (GiveRefine (..))
import Agda.Syntax.Common (InteractionId)
import Data.Bifunctor (bimap)
import Data.Text (Text)

import AgdaMCP.Interaction.GiveFamily (
  expectComputed,
  giveGen',
  withResolvedHole,
 )
import AgdaMCP.Interaction.Internal (
  InteractionM,
  runCommandMWithLoadedFile,
 )
import AgdaMCP.Interaction.Model (GiveError (..), NoLoadedFile, Span)

{- | Elaborate-give always sets the force parameter to false, so we do not
include that in the request.
-}
data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  , requestExpression :: Text
  }

{- | The result is always Agda's own elaboration of the expression, never the
caller's text (see `expectComputed`), so this is a `Text` rather than
`GiveAction`. The `Span` is the hole it was elaborated for.
-}
type Response = Either GiveError (Span, Text)

elaborateGive :: Request -> InteractionM (Either NoLoadedFile Response)
elaborateGive = runCommandMWithLoadedFile . elaborateGiveInternal

elaborateGiveInternal :: Request -> CommandM Response
elaborateGiveInternal (Request normalization goalId expression) =
  withResolvedHole GiveUnknownId goalId $ \range holeSpan -> do
    outcome <-
      giveGen' WithoutForce (ElaborateGive normalization) goalId range expression
    bimap (GiveFailed holeSpan) (holeSpan,)
      <$> traverse (expectComputed goalId) outcome
