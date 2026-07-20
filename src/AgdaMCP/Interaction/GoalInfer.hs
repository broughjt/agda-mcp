module AgdaMCP.Interaction.GoalInfer (
  Request (..),
  Response (..),
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
import Agda.TypeChecking.Monad (TCErr, withInteractionId)
import Control.Monad.State (lift)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Goal (extractGoalReport)
import AgdaMCP.Interaction.Internal (InteractionM, catchTCErr, runCommandM)
import AgdaMCP.Interaction.Model (
  Error,
  GoalReport,
  extractError,
  matchNoSuchInteractionPoint,
 )

data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  , requestExpression :: Text
  }
  deriving (Eq, Show)

data Response
  = -- The goal report together with the inferred type of the expression.
    ResponseOk GoalReport Have
  | -- The requested goal id did not correspond to an interaction point.
    ResponseUnknownId InteractionId
  | -- Any other `TCErr`, including every way the expression can be at fault
    -- (parse error, unbound name, ill-typed).
    ResponseError Error
  deriving (Eq, Show)

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
goalInferInternal (Request norm goalId expression) =
  ( do
      -- `interpret Cmd_goal_type_context_infer` (InteractionTop.hs:727-738),
      -- with the blank-expression fallback to `Cmd_goal_type_context`
      -- purposefully left out. The idea is that this wrapper always infers, and
      -- we can leave policy decisions about what to do with an empty
      -- expressions to the layer above.
      (inferredType, faces) <-
        liftLocalState $ withInteractionId goalId $ do
          parsed <- parseExprIn goalId noRange (Text.unpack expression)
          typeAndFacesInMeta goalId norm parsed
      -- Rendered at display scope (no `withInteractionId`), matching EmacsTop's
      -- `auxDoc` (:235-237): `prettyATop` the type, plain `pretty` each face.
      have <-
        liftLocalState $ do
          typeDoc <- prettyATop inferredType
          pure $
            Have
              (Text.pack $ render typeDoc)
              (map (Text.pack . render . pretty) faces)
      report <- liftLocalState $ extractGoalReport norm goalId
      pure $ ResponseOk report have
  )
    `catchTCErr` handler
 where
  handler :: TCErr -> CommandM Response
  handler e = case matchNoSuchInteractionPoint e of
    Just badId -> pure $ ResponseUnknownId badId
    Nothing -> ResponseError <$> lift (extractError e)
