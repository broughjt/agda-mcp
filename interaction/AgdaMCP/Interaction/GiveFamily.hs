module AgdaMCP.Interaction.GiveFamily (
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
import Agda.Interaction.Response (Response_boot (Resp_GiveAction))
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Position (Range)
import Agda.TypeChecking.Monad (
  TCErr,
  getInteractionOutputCallback,
  setInteractionOutputCallback,
 )
import Agda.TypeChecking.Monad.MetaVars (getInteractionRange)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Extract (
  classifyInteractionError,
  extractError,
  rangeSpan,
  toGiveAction,
 )
import AgdaMCP.Interaction.Internal (catchTCErr)
import AgdaMCP.Interaction.Model (
  Error,
  GiveAction (..),
  Span,
 )

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

@Left@ is a bogus id, and it is the only way this lookup can fail in a way a
caller can act on. @getInteractionRange@ is @ipRange <.>
lookupInteractionPoint@, whose one error is @NoSuchInteractionPoint@
(MetaVars.hs). Any other @TCErr@ means our model of Agda is wrong, so we
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

extractHoleSpan :: InteractionId -> Range -> CommandM Span
extractHoleSpan goalId =
  -- An interaction point carrying no range means our model of Agda is wrong, the
  -- same claim @AgdaMCP.Interaction.Extract.extractGoal@ makes about goals.
  maybe (liftIO $ throwIO $ HoleNoRange goalId) pure . rangeSpan

{- | Run Agda's @give_gen@ and recover its payload from the response it emits.

This is a shared driver for give/refine/intro/elaborate-give, mirroring how all
of Agda's give-family @interpret@ clauses forward to @give_gen@
(InteractionTop.hs). The goal id is the request point, used to
sanity-check the id in the captured action, and its range comes from
@resolveHole@. A caller that resolved the hole first has already ruled out an
unknown id, so a @TCErr@ here is the command itself failing.

Deliberate hack:

One goal of the interaction layer is to avoid needing to intercept and parse
@Resp_@ streams. However, @give_gen@ is a complicated and delicate looking
helper whose logic I don't wish to duplicate. Its output is not written to
@TCState@ in any form, so we can't recover the needed information by querying
that state after the call. Rather than reimplement @give_gen@ myself I have
chosen to use the response output callback in this one instance.
-}
giveGen' ::
  UseForce ->
  GiveRefine ->
  InteractionId ->
  Range ->
  Text ->
  CommandM (Either Error GiveAction)
giveGen' force giveRefine goalId range expression = do
  slot <- liftIO $ newIORef Nothing
  installed <- lift getInteractionOutputCallback
  lift $ setInteractionOutputCallback $ \response -> do
    case response of
      Resp_GiveAction pointId giveResult ->
        liftIO $ writeIORef slot $ Just (pointId, giveResult)
      _ -> pure ()
    installed response
  outcome <-
    (Right <$> give_gen force goalId range (Text.unpack expression) giveRefine)
      `catchTCErr` (fmap Left . lift . extractError)
  -- @catchTCErr@ turns every @TCErr@ but @PatternErr@ into a @Left@, so this
  -- runs on both outcomes. Any exception that skips it also escapes
  -- @runCommandM@, which discards this command's copy of the session, callback
  -- included.
  lift $ setInteractionOutputCallback installed
  case outcome of
    Left e -> pure $ Left e
    Right () -> do
      captured <- liftIO $ readIORef slot
      case captured of
        Just (actionId, payload) | actionId == goalId -> pure $ Right $ toGiveAction payload
        -- `give_gen` returned `()` but emitted no give action for `goalId`. It
        -- only no-ops on empty input (guarded at the tool layer) or if our
        -- mental model of Agda is wrong, so we should die loudly,
        _ -> liftIO $ throwIO $ NoGiveAction goalId

-- The @give_gen@ helper only keeps the user's text verbatim for give and refine
-- (@literally@, InteractionTop.hs). In contrast, intro and elaborate-give
-- always return a @Give_String@. If Agda ever produces @Give_Paren@ or
-- @Give_NoParen@ for the intro or elaborate-give commands, our understanding of
-- @give_gen@ is wrong, in which case we should die loudly.
expectComputed :: InteractionId -> GiveAction -> CommandM Text
expectComputed _ (GiveComputed text) = pure text
expectComputed goalId giveAction@(GiveVerbatim _) =
  liftIO $ throwIO $ NotComputed goalId giveAction

-- Bug: @give_gen@ succeeded but produced no give action for the requested
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
-- the id being unknown. See @resolveHole@.
data HoleUnresolved
  = HoleNoRange InteractionId
  | HoleLookupFailed InteractionId Error
  deriving (Show)

instance Exception HoleUnresolved
