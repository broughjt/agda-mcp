module AgdaMCP.Interaction.Goal (
  Request (..),
  Response,
  goal,
  extractGoalReport,
) where

import Agda.Interaction.Base (OutputConstraint_boot (..), Rewrite)
import Agda.Interaction.BasicOps (
  getConstraintsMentioning,
  getIPBoundary,
  typeOfMeta,
 )
import Agda.Interaction.Command (CommandM, liftLocalState)
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Common.Pretty (pretty, render)
import Agda.TypeChecking.Monad (TCM, lookupInteractionId)
import Agda.TypeChecking.Pretty (prettyTCM)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Context (extractContext)
import AgdaMCP.Interaction.Extract (classifyInteractionError)
import AgdaMCP.Interaction.Internal (InteractionM, catchTCErr, runCommandM)
import AgdaMCP.Interaction.Model (
  GoalError (..),
  GoalReport (..),
  GoalShape (..),
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

{- | Following the body of @cmd_goal_type_context_and@
(InteractionTop.hs:1063-1067) plus the goal shape both frontends re-query at
render time (EmacsTop.hs:227, JSONTop.hs:395). Shared as a helper for the
infer and check wrappers.
-}
extractGoalReport :: Rewrite -> InteractionId -> TCM GoalReport
extractGoalReport normalization goalId = do
  context <- extractContext normalization goalId
  shape <- extractGoalShape normalization goalId
  -- @getIPBoundary@ returns the wanted faces. Both frontends render each face
  -- with @pretty@ (EmacsTop.hs:228-232, JSONTop.hs:398 uses @encodePretty =
  -- encodeShow . pretty@). Empty when the goal is non-cubical.
  boundary <-
    map (Text.pack . render . pretty) <$> getIPBoundary normalization goalId
  -- Constraints mentioning this goal's metavariable. Emacs renders these with
  -- @prettyTCM@ (EmacsTop.hs:245) while JSON uses @pretty@. Apparently the the
  -- two can differ; we follow Emacs.
  constraints <-
    lookupInteractionId goalId
      >>= getConstraintsMentioning normalization
      >>= traverse (fmap (Text.pack . render) . prettyTCM)
  pure $ GoalReport shape boundary context constraints

-- | Obtain the goal shape mirroring @prettyTypeOfMeta@ (EmacsTop.hs:378-382)
extractGoalShape :: Rewrite -> InteractionId -> TCM GoalShape
extractGoalShape normalization goalId = do
  form <- typeOfMeta normalization goalId
  case form of
    OfType _ ty -> GoalOfType . Text.pack . render <$> prettyATop ty
    JustSort _ -> pure GoalSort
    -- @typeOfMeta@ only ever builds @OfType@/@JustSort@ (see the @GoalShape@
    -- comment in Model.hs). Anything else means our model of Agda is wrong.
    _ ->
      prettyATop form
        >>= liftIO
          . throwIO
          . UnexpectedGoalConstraint
          . Text.pack
          . render

newtype GoalBug = UnexpectedGoalConstraint Text
  deriving (Show)

instance Exception GoalBug
