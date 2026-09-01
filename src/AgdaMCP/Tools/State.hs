module AgdaMCP.Tools.State (
  ToolState (..),
  ToolM,
  newToolState,
  liftInteraction,
  runToolM,
  withBudget,
) where

import Agda.Interaction.Options (CommandLineOptions)
import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Monad.State (StateT (..))
import Data.Bifunctor (first)
import System.Timeout (timeout)

import AgdaMCP.Interaction (
  InteractionM,
  InteractionState,
  newInteractionState,
 )
import AgdaMCP.Tools.LoadId (LoadGeneration (..), newSessionNonce)
import Data.Tuple (swap)

{- | Tool state, consisting of interaction-layer state, held opaquely, and state
for tracking load generations.
-}
data ToolState = ToolState
  { toolInteractionState :: InteractionState
  , toolLoadGeneration :: LoadGeneration
  }

type ToolM = StateT ToolState IO

newToolState :: CommandLineOptions -> IO ToolState
newToolState options = do
  nonce <- newSessionNonce
  interactionState <- newInteractionState options
  pure
    ToolState
      { toolInteractionState = interactionState
      , toolLoadGeneration =
          LoadGeneration
            { sessionNonce = nonce
            , loadsIssued = 0
            , currentLoad = Nothing
            }
      }

{- | Run an interaction-layer action on the session inside @ToolState@, writing
the successor session back.
-}
liftInteraction :: InteractionM a -> ToolM a
liftInteraction action = StateT $ \state -> do
  (result, interactionState) <- runStateT action $ toolInteractionState state
  pure (result, state {toolInteractionState = interactionState})

-- Run a @ToolM@ action against the state held in the @MVar@, storing the
-- successor state back. The transport runs each request in its own task, so
-- the @MVar@ is what serializes tool calls.
--
-- Exceptions thrown mid-action, such as a bug exception or a cancellation
-- exception, will skip the put and propagate out of the tool handler, leaving
-- the pre-call state in place. We deliberately catch it nowhere, so a tool call
-- that dies discards its whole state change.
runToolM :: MVar ToolState -> ToolM a -> IO a
runToolM stateVariable action =
  modifyMVar stateVariable $ fmap swap . runStateT action

{- | Run an action under a budget, in microseconds, answering @Nothing@ and
discarding its state change if it runs longer.
-}
withBudget :: Int -> ToolM a -> ToolM (Maybe a)
withBudget budget action = StateT $ \state ->
  maybe (Nothing, state) (first Just)
    <$> timeout budget (runStateT action state)
