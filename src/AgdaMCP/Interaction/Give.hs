module AgdaMCP.Interaction.Give (
  Request (..),
  Response,
  give,
  giveInternal,
  -- Shared with the rest of the give family of commands.
  giveGen',
  expectComputed,
  NoGiveAction (..),
  NotComputed (..),
) where

import Agda.Interaction.Base (UseForce (..))
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (GiveRefine (..), give_gen)
import Agda.Syntax.Common (InteractionId)
import Agda.TypeChecking.Monad.MetaVars (getInteractionRange)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, lift)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Internal (
  GiveSlot,
  InteractionM,
  InteractionState (..),
  catchTCErr,
  runCommandM,
 )
import AgdaMCP.Interaction.Model (
  GiveAction (..),
  GiveError (..),
  classifyInteractionError,
  toGiveAction,
 )

data Request = Request
  { requestForce :: UseForce
  , requestGoalId :: InteractionId
  , requestExpression :: Text
  }

-- `Left` is a bad id or a `TCErr`, while `Right` is the action to apply in the
-- hole. Shared by `Refine` and `ElaborateGive`, which have identical failure
-- modes.
type Response = Either GiveError GiveAction

give :: Request -> InteractionM Response
give request = do
  InteractionState _ _ slot <- get
  runCommandM (giveInternal slot request)

giveInternal :: GiveSlot -> Request -> CommandM Response
giveInternal slot (Request force goalId s) =
  giveGen' slot force Give goalId s

{- | Run Agda's `give_gen` and (hackily) recover its payload via the capturing
callback (see `AgdaMCP.Interaction.Internal.captureGiveAction`).

This is a shared driver for `Give`/`Refine`/`Intro`/`ElaborateGive`, mirroring
how all of Agda's give-family `interpret` clauses forward to `give_gen`
(InteractionTop.hs:948-1033). The `goalId` argument is the request point, used
to sanity-check the id in the captured action, and the hole's `Range` is
recovered from it (see below).
-}
giveGen' ::
  GiveSlot ->
  UseForce ->
  GiveRefine ->
  InteractionId ->
  Text ->
  CommandM (Either GiveError GiveAction)
giveGen' slot force giveRefine goalId expression =
  ( do
      -- The hole's range can be obtained from `goalId` and the current state.
      -- Emacs passes the buffer range because it owns the buffer positions, but
      -- we have no drifting buffer (we intend to reload after each give), so the
      -- interaction point's own stored range can be trusted.
      range <- lift $ getInteractionRange goalId
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
    `catchTCErr` (fmap Left . lift . classifyInteractionError GiveUnknownId GiveFailed)

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
