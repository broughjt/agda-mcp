module AgdaMCP.Interaction.Commands.Intro (
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
import Control.Monad.State (lift)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Extract (extractError)
import AgdaMCP.Interaction.GiveFamily (
  expectComputed,
  giveGen',
  withResolvedHole,
 )
import AgdaMCP.Interaction.Internal (
  InteractionM,
  catchTCErr,
  runCommandMWithLoadedFile,
 )
import AgdaMCP.Interaction.Model (
  IntroError (..),
  NoLoadedFile,
  Span,
 )

{- | Intro takes no user expression, since @introTactic@ proposes the
candidate(s) (InteractionTop.hs).
-}
data Request = Request
  { requestPatternLambda :: Bool
  , requestGoalId :: InteractionId
  }

-- | Success is the hole and the introduction form Agda chose.
type Response = Either IntroError (Span, Text)

intro :: Request -> InteractionM (Either NoLoadedFile Response)
intro = runCommandMWithLoadedFile . introInternal

introInternal :: Request -> CommandM Response
introInternal (Request patternLambda goalId) =
  withResolvedHole IntroUnknownId goalId $
    introInHole patternLambda goalId

introInHole ::
  Bool -> InteractionId -> Range -> Span -> CommandM Response
introInHole patternLambda goalId range holeSpan =
  -- Follows @interpret Cmd_intro@ (InteractionTop.hs). @introTactic@
  -- runs outside @withInteractionId@ and the give runs inside it. Mirror the
  -- split exactly.
  ( do
      candidates <- lift $ introTactic patternLambda goalId
      liftCommandMT (withInteractionId goalId) $ case candidates of
        [] -> pure $ Left $ IntroNotFound holeSpan
        [s] -> do
          outcome <- giveGen' WithoutForce Intro goalId range $ Text.pack s
          bimap (IntroFailed holeSpan) (holeSpan,)
            <$> traverse (expectComputed goalId) outcome
        _ ->
          pure $ Left $ IntroAmbiguous holeSpan $ map Text.pack candidates
  )
    `catchTCErr` (fmap (Left . IntroFailed holeSpan) . lift . extractError)
