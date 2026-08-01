module AgdaMCP.Interaction.GoalInfer (
  Request (..),
  Response,
  Have (..),
  goalInfer,
) where

import Agda.Interaction.Base (Rewrite)
import Agda.Interaction.BasicOps (parseExprIn, typeAndFacesInMeta)
import Agda.Interaction.Command (CommandM, liftLocalState)
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Common.Pretty (pretty, render)
import Agda.Syntax.Position (noRange)
import Agda.TypeChecking.Monad (withInteractionId)
import Control.Monad.State (lift)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Goal (extractGoalReport)
import AgdaMCP.Interaction.Extract (classifyInteractionError)
import AgdaMCP.Interaction.Internal (InteractionM, catchTCErr, runCommandM)
import AgdaMCP.Interaction.Model (
  GoalError (..),
  GoalReport,
 )

data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  , requestExpression :: Text
  }
  deriving (Eq, Show)

-- `Left` is a bad id or a `TCErr` (every way the expression can be at
-- fault--for example, parse error, unbound name, ill-typed), while `Right`
-- pairs the goal report with the inferred "Have" of the user's expression.
type Response = Either GoalError (GoalReport, Have)

-- The "Have:" part of the goal display (EmacsTop.hs:235-237), consisting of the
-- inferred type of the user's expression and its actual boundary faces.
data Have = Have
  { haveType :: Text
  , haveBoundary :: [Text]
  }
  deriving (Eq, Show)

goalInfer :: Request -> InteractionM Response
goalInfer = runCommandM . goalInferInternal

goalInferInternal :: Request -> CommandM Response
goalInferInternal (Request normalization goalId expression) =
  ( do
      -- `interpret Cmd_goal_type_context_infer` (InteractionTop.hs:727-738),
      -- with the blank-expression fallback to `Cmd_goal_type_context`
      -- purposefully left out. The idea is that this wrapper always infers, and
      -- we can leave policy decisions about what to do with an empty
      -- expressions to the layer above.
      (inferredType, faces) <-
        liftLocalState $ withInteractionId goalId $ do
          parsed <- parseExprIn goalId noRange (Text.unpack expression)
          typeAndFacesInMeta goalId normalization parsed
      -- Rendered at display scope (no `withInteractionId`), matching EmacsTop's
      -- `auxDoc` (:235-237): `prettyATop` the type, plain `pretty` each face.
      have <-
        liftLocalState $ do
          typeDoc <- prettyATop inferredType
          pure $
            Have
              (Text.pack $ render typeDoc)
              (map (Text.pack . render . pretty) faces)
      report <- liftLocalState $ extractGoalReport normalization goalId
      pure $ Right (report, have)
  )
    `catchTCErr` (fmap Left . lift . classifyInteractionError GoalUnknownId GoalFailed)
