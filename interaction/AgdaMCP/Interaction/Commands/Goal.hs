module AgdaMCP.Interaction.Commands.Goal (
  Request (..),
  Response,
  goal,
) where

import Agda.Interaction.Base (Rewrite)
import Agda.Interaction.Command (CommandM, liftLocalState)
import Agda.Syntax.Common (InteractionId)
import Control.Monad.State (lift)

import AgdaMCP.Interaction.Extract (classifyInteractionError, extractGoalReport)
import AgdaMCP.Interaction.Internal (InteractionM, catchTCErr, runCommandM)
import AgdaMCP.Interaction.Model (
  GoalError (..),
  GoalReport (..),
 )

data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  }
  deriving (Eq, Show)

type Response = Either GoalError GoalReport

goal :: Request -> InteractionM Response
goal = runCommandM . goalInternal

goalInternal :: Request -> CommandM Response
goalInternal (Request normalization goalId) =
  -- @interpret Cmd_goal_type_context@ (InteractionTop.hs:724-725) with
  -- @GoalOnly@. Like the context command we run the query under
  -- @liftLocalState@ since a display command should not modify @TCState@.
  (Right <$> liftLocalState (extractGoalReport normalization goalId))
    `catchTCErr` (fmap Left . lift . classifyInteractionError GoalUnknownId GoalFailed)
