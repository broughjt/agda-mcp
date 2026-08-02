module AgdaMCP.Interaction.Intro (
  Request (..),
  Response,
  intro,
  introInternal,
) where

import Agda.Interaction.Base (UseForce (..))
import Agda.Interaction.BasicOps (introTactic)
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (GiveRefine (..), liftCommandMT)
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Position (Range)
import Agda.TypeChecking.Monad (withInteractionId)
import Control.Monad.State (get, lift)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Extract (extractError)
import AgdaMCP.Interaction.Give (
  expectComputed,
  giveGen',
  withResolvedHole,
 )
import AgdaMCP.Interaction.Internal (
  GiveSlot,
  InteractionM,
  InteractionState (..),
  catchTCErr,
  runCommandM,
 )
import AgdaMCP.Interaction.Model (
  IntroError (..),
  Span,
 )

-- Intro takes no user expression, since `introTactic` proposes the candidate(s)
-- (InteractionTop.hs:653-660).
data Request = Request
  { requestPatternLambda :: Bool
  , requestGoalId :: InteractionId
  }

-- `Left` collects every reason no action was produced--a bad ID, a `TCErr`, or
-- `introTactic` finding no/several candidates. `Right` is the hole and the
-- introduction form Agda chose.
type Response = Either IntroError (Span, Text)

intro :: Request -> InteractionM Response
intro request = do
  InteractionState _ _ slot <- get
  runCommandM (introInternal slot request)

introInternal :: GiveSlot -> Request -> CommandM Response
introInternal slot (Request patternLambda goalId) =
  withResolvedHole IntroUnknownId goalId $
    introInHole slot patternLambda goalId

introInHole ::
  GiveSlot -> Bool -> InteractionId -> Range -> Span -> CommandM Response
introInHole slot patternLambda goalId range holeSpan =
  -- `interpret Cmd_intro` (InteractionTop.hs:653-660). `introTactic` runs
  -- outside `withInteractionId` and the give runs inside it. Mirror the split
  -- exactly.
  ( do
      candidates <- lift $ introTactic patternLambda goalId
      liftCommandMT (withInteractionId goalId) $ case candidates of
        [] -> pure $ Left $ IntroNotFound holeSpan
        [s] -> do
          outcome <- giveGen' slot WithoutForce Intro goalId range $ Text.pack s
          bimap (IntroFailed holeSpan) (holeSpan,)
            <$> traverse (expectComputed goalId) outcome
        _ ->
          pure $ Left $ IntroAmbiguous holeSpan $ map Text.pack candidates
  )
    `catchTCErr` (fmap (Left . IntroFailed holeSpan) . lift . extractError)
