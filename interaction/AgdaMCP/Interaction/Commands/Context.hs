module AgdaMCP.Interaction.Commands.Context (
  Request (..),
  Response,
  context,
) where

import Agda.Interaction.Base (Rewrite)
import Agda.Interaction.Command (CommandM, liftLocalState)
import Agda.Syntax.Common (InteractionId)
import Control.Monad.State (lift)

import AgdaMCP.Interaction.Extract (classifyInteractionError, extractContext)
import AgdaMCP.Interaction.Internal (InteractionM, catchTCErr, runCommandM)
import AgdaMCP.Interaction.Model (
  ContextEntry (..),
  GoalError (..),
 )

data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  }
  deriving (Eq, Show)

type Response = Either GoalError [ContextEntry]

context :: Request -> InteractionM Response
context = runCommandM . contextInternal

contextInternal :: Request -> CommandM Response
contextInternal (Request normalization goalId) =
  -- The meat of @interpret Cmd_context@ (InteractionTop.hs:706-707) is
  -- @getResponseContext@. We run the query under @liftLocalState@, the idea
  -- being that display commands should not modify @TCState@ (apparently
  -- reification can allocate metavariables, etc.). The Emacs renderer runs
  -- under @localTCState@ for the same reason (EmacsTop.hs:212). A non-existent
  -- id is classified by @classifyInteractionError@; any other @TCErr@ becomes
  -- @GoalFailed@.
  (Right <$> liftLocalState (extractContext normalization goalId))
    `catchTCErr` (fmap Left . lift . classifyInteractionError GoalUnknownId GoalFailed)
