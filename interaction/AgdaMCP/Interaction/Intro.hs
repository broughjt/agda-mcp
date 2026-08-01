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
import Agda.TypeChecking.Monad (withInteractionId)
import Control.Monad.State (get, lift)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Give (expectComputed, giveGen')
import AgdaMCP.Interaction.Internal (
  GiveSlot,
  InteractionM,
  InteractionState (..),
  catchTCErr,
  runCommandM,
 )
import AgdaMCP.Interaction.Model (
  GiveError (..),
  IntroError (..),
  classifyInteractionError,
 )

-- Intro takes no user expression, since `introTactic` proposes the candidate(s)
-- (InteractionTop.hs:653-660).
data Request = Request
  { requestPatternLambda :: Bool
  , requestGoalId :: InteractionId
  }

-- `Left` collects every reason no action was produced--a bad ID, a `TCErr`, or
-- `introTactic` finding no/several candidates. `Right` is the introduction form
-- Agda chose: intro is handed no text of its own, so it can only ever splice
-- Agda's (see `expectComputed`).
type Response = Either IntroError Text

intro :: Request -> InteractionM Response
intro request = do
  InteractionState _ _ slot <- get
  runCommandM (introInternal slot request)

introInternal :: GiveSlot -> Request -> CommandM Response
introInternal slot (Request patternLambda goalId) =
  -- `interpret Cmd_intro` (InteractionTop.hs:653-660). `introTactic` runs
  -- outside `withInteractionId` and the give runs inside it. Mirror the split
  -- exactly.
  ( do
      candidates <- lift $ introTactic patternLambda goalId
      liftCommandMT (withInteractionId goalId) $ case candidates of
        [] -> pure $ Left IntroNotFound
        [s] -> do
          giveAction <- giveGen' slot WithoutForce Intro goalId $ Text.pack s
          first fromGiveError <$> traverse (expectComputed goalId) giveAction
        _ -> pure $ Left $ IntroAmbiguous $ map Text.pack candidates
  )
    `catchTCErr` (fmap Left . lift . classifyInteractionError IntroUnknownId IntroFailed)

-- Embed the shared driver's `GiveError` into intro's richer error type. The
-- `NotFound`/`Ambiguous` cases never come from a give, so they are absent here.
fromGiveError :: GiveError -> IntroError
fromGiveError (GiveUnknownId goalId) = IntroUnknownId goalId
fromGiveError (GiveFailed err) = IntroFailed err
