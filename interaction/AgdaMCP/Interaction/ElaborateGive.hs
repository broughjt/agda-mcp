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
import Data.Bifunctor (bimap)
import Data.Text (Text)

import AgdaMCP.Interaction.Give (
  expectComputed,
  giveGen',
  withResolvedHole,
 )
import AgdaMCP.Interaction.Internal (
  GiveSlot,
  InteractionM,
  InteractionState (..),
  runCommandM,
 )
import AgdaMCP.Interaction.Model (GiveError (..), Span)

-- Elaborate-give always sets the force parameter to false, so we do not include
-- that in the request. It does carry a normalization mode for the elaborated
-- term.
data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  , requestExpression :: Text
  }

-- The result is always Agda's own elaboration of the expression, never the
-- caller's text (see `expectComputed`), so this is a `Text` rather than
-- `GiveAction`. The `Span` is the hole it was elaborated for.
type Response = Either GiveError (Span, Text)

elaborateGive :: Request -> InteractionM Response
elaborateGive request = do
  InteractionState _ _ slot <- get
  runCommandM $ elaborateGiveInternal slot request

elaborateGiveInternal :: GiveSlot -> Request -> CommandM Response
elaborateGiveInternal slot (Request normalization goalId expression) =
  withResolvedHole GiveUnknownId goalId $ \range holeSpan -> do
    outcome <-
      giveGen' slot WithoutForce (ElaborateGive normalization) goalId range expression
    bimap (GiveFailed holeSpan) (holeSpan,)
      <$> traverse (expectComputed goalId) outcome
