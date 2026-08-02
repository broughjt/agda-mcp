module AgdaMCP.Interaction.Give (
  Request (..),
  Response,
  give,
  giveInternal,
  -- Shared with the rest of the give family of commands.
  withResolvedHole,
  giveGen',
  expectComputed,
  NoGiveAction (..),
  NotComputed (..),
  HoleUnresolved (..),
) where

import Agda.Interaction.Base (UseForce (..))
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (GiveRefine (..), give_gen)
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Position (Range)
import Agda.TypeChecking.Monad (TCErr)
import Agda.TypeChecking.Monad.MetaVars (getInteractionRange)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, lift)
import Data.Bifunctor (bimap)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Extract (
  classifyInteractionError,
  extractError,
  rangeSpan,
  toGiveAction,
 )
import AgdaMCP.Interaction.Internal (
  GiveSlot,
  InteractionM,
  InteractionState (..),
  catchTCErr,
  runCommandM,
 )
import AgdaMCP.Interaction.Model (
  Error,
  GiveAction (..),
  GiveError (..),
  Span,
 )

data Request = Request
  { requestForce :: UseForce
  , requestGoalId :: InteractionId
  , requestExpression :: Text
  }

-- `Left` is a bad id or a `TCErr`, while `Right` is the hole that was filled
-- and the action to apply in it. Shared by `Refine` and `ElaborateGive`, which
-- have identical failure modes.
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

withResolvedHole ::
  (InteractionId -> e) ->
  InteractionId ->
  (Range -> Span -> CommandM (Either e a)) ->
  CommandM (Either e a)
withResolvedHole unknownId goalId k =
  resolveHole goalId
    >>= either
      (pure . Left . unknownId)
      (\range -> extractHoleSpan goalId range >>= k range)

{- | Look up the hole an interaction id names, which every command in the give
family does before running.

`Left` is a bogus id, and it is the only way this lookup can fail in a way a
caller can act on. `getInteractionRange` is `ipRange <.>
lookupInteractionPoint`, whose one error is `NoSuchInteractionPoint`
(MetaVars.hs:635-640). Any other `TCErr` means our model of Agda is wrong, so we
die loudly.
-}
resolveHole :: InteractionId -> CommandM (Either InteractionId Range)
resolveHole goalId =
  (Right <$> lift (getInteractionRange goalId)) `catchTCErr` handler
 where
  handler :: TCErr -> CommandM (Either InteractionId Range)
  handler e =
    lift (classifyInteractionError Left Right e)
      >>= either
        (pure . Left)
        (liftIO . throwIO . HoleLookupFailed goalId)

-- An interaction point carrying no range means our model of Agda is wrong, the
-- same claim `AgdaMCP.Interaction.Metas.extractGoal` makes about goals.
extractHoleSpan :: InteractionId -> Range -> CommandM Span
extractHoleSpan goalId =
  maybe (liftIO $ throwIO $ HoleNoRange goalId) pure . rangeSpan

{- | Run Agda's `give_gen` and (hackily) recover its payload via the capturing
callback (see `AgdaMCP.Interaction.Internal.captureGiveAction`).

This is a shared driver for `Give`/`Refine`/`Intro`/`ElaborateGive`, mirroring
how all of Agda's give-family `interpret` clauses forward to `give_gen`
(InteractionTop.hs:948-1033). The goal id is the request point, used to
sanity-check the id in the captured action, and its range comes from
`resolveHole`. A caller that resolved the hole first has already ruled out an
unknown id, so a `TCErr` here is the command itself failing.
-}
giveGen' ::
  GiveSlot ->
  UseForce ->
  GiveRefine ->
  InteractionId ->
  Range ->
  Text ->
  CommandM (Either Error GiveAction)
giveGen' slot force giveRefine goalId range expression =
  ( do
      -- Safety: we clear the slot before and read after. Since handlers execute
      -- sequentially, there won't be a window where the obtain result can be
      -- stale.
      liftIO $ writeIORef slot Nothing
      give_gen force goalId range (Text.unpack expression) giveRefine
      captured <- liftIO $ readIORef slot
      case captured of
        Just (actionId, payload) | actionId == goalId -> pure $ Right $ toGiveAction payload
        -- `give_gen` returned `()` but emitted no give action for `goalId`. It
        -- only no-ops on empty input (guarded at the tool layer) or if our
        -- mental model of Agda is wrong, so we should die loudly,
        _ -> liftIO $ throwIO $ NoGiveAction goalId
  )
    `catchTCErr` (fmap Left . lift . extractError)

-- `give_gen` only keeps the user's own text for `Give` and `Refine`
-- (`literally`, InteractionTop.hs:1011), so `Intro` and `ElaborateGive` always
-- report a `Give_String`. Their callers have nothing to do with a
-- `GiveVerbatim`--intro is handed no text to keep, and elaborating exists
-- precisely to replace what the caller wrote. If Agda ever produces it anyway
-- our model of `give_gen` is wrong, so die loudly.
expectComputed :: InteractionId -> GiveAction -> CommandM Text
expectComputed _ (GiveComputed text) = pure text
expectComputed goalId giveAction@(GiveVerbatim _) =
  liftIO $ throwIO $ NotComputed goalId giveAction

-- Bug: `give_gen` succeeded but produced no give action for the requested
-- point.
newtype NoGiveAction = NoGiveAction InteractionId
  deriving (Show)

instance Exception NoGiveAction

-- Bug: a command that can only splice Agda's own text reported the caller's
-- text as literal.
data NotComputed = NotComputed InteractionId GiveAction
  deriving (Show)

instance Exception NotComputed

-- Bug: an interaction point that cannot be turned into a hole, either because
-- it carries no range or because looking it up failed for a reason other than
-- the id being unknown. See `resolveHole`.
data HoleUnresolved
  = HoleNoRange InteractionId
  | HoleLookupFailed InteractionId Error
  deriving (Show)

instance Exception HoleUnresolved
