module AgdaMCP.Interaction.GoalCheck (
  Request (..),
  Response,
  goalCheck,
) where

import Agda.Interaction.Base (OutputConstraint_boot (..), Rewrite (..))
import Agda.Interaction.BasicOps (normalForm, parseExprIn, typeOfMeta)
import Agda.Interaction.Command (CommandM, liftLocalState)
import Agda.Syntax.Abstract (Expr)
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Common.Pretty (render)
import Agda.Syntax.Position (noRange)
import Agda.Syntax.Translation.InternalToAbstract (reify)
import Agda.TypeChecking.Monad (withInteractionId)
import Agda.TypeChecking.Rules.Term (checkExpr, isType_)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Goal (extractGoalReport)
import AgdaMCP.Interaction.Internal (InteractionM, catchTCErr, runCommandM)
import AgdaMCP.Interaction.Model (
  GoalError (..),
  GoalReport,
  classifyInteractionError,
 )

data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  , requestExpression :: Text
  }
  deriving (Eq, Show)

-- `Left` is a bad id or a `TCErr` (every way the expression can be at
-- fault--for example, parse error, unbound name, ill-typed against the goal),
-- while `Right` pairs the goal report with the elaborated ("Elaborates to:")
-- term.
type Response = Either GoalError (GoalReport, Text)

goalCheck :: Request -> InteractionM Response
goalCheck = runCommandM . goalCheckInternal

goalCheckInternal :: Request -> CommandM Response
goalCheckInternal (Request normalization goalId expression) =
  ( do
      -- `interpret Cmd_goal_type_context_check` (InteractionTop.hs:740-748).
      -- Parses the expression, checks it against the goal type (queried `AsIs`,
      -- deliberately), and normalizes/reifies the elaborated term.
      elaborated <-
        liftLocalState $ withInteractionId goalId $ do
          expression' <- parseExprIn goalId noRange $ Text.unpack expression
          goalType <- typeOfMeta AsIs goalId
          term <- case goalType of
            OfType _ ty -> checkExpr expression' =<< isType_ ty
            -- Agda asserts `__IMPOSSIBLE__`s here (:746). A term cannot be
            -- checked against a sort-shaped goal. Otherwise our mental model of
            -- Agda is wrong, so we die loudly and signal a bug.
            _ -> liftIO $ throwIO $ CannotCheckAgainstNonType goalId
          reify =<< normalForm normalization term
      -- Render matching EmacsTop's `auxDoc` (:238-240).
      have <- liftLocalState $ Text.pack . render <$> prettyATop (elaborated :: Expr)
      report <- liftLocalState $ extractGoalReport normalization goalId
      pure $ Right (report, have)
  )
    `catchTCErr` (fmap Left . lift . classifyInteractionError GoalUnknownId GoalFailed)

newtype GoalCheckBug = CannotCheckAgainstNonType InteractionId
  deriving (Show)

instance Exception GoalCheckBug
