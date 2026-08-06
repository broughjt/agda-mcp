module AgdaMCP.Interaction.Metas (
  Request (..),
  Response,
  metas,
  extractMetas,
) where

import Agda.Interaction.Base (OutputConstraint_boot (..), Rewrite (..))
import Agda.Interaction.BasicOps (
  getWarningsAndNonFatalErrors,
  typesOfHiddenMetas,
  typesOfVisibleMetas,
 )
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.Output (OutputConstraint)
import Agda.Syntax.Abstract (Expr)
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Common.Pretty (prettyShow, render)
import Agda.TypeChecking.Monad (
  NamedMeta,
  TCM,
  WarningsAndNonFatalErrors (..),
  getMetaRange,
  nmid,
  withInteractionId,
  withMetaId,
 )
import Agda.TypeChecking.Monad.MetaVars (
  getInteractionPoints,
  getInteractionRange,
 )
import Agda.TypeChecking.Pretty.Warning (filterTCWarnings)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Extract (
  extractError,
  extractNonFatalError,
  extractWarning,
  rangeSpan,
 )
import AgdaMCP.Interaction.Internal (InteractionM, catchTCErr, runCommandM)
import AgdaMCP.Interaction.Model (
  Error,
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  MetasReport (..),
  Position (..),
  Span (..),
 )

-- | @Cmd_metas@ takes the degree of normalization to render goal types at.
data Request = Request {requestNormalization :: Rewrite}
  deriving (Eq, Show)

type Response = Either Error MetasReport

metas :: Request -> InteractionM Response
metas = runCommandM . metasInternal

metasInternal :: Request -> CommandM Response
metasInternal (Request normalization) =
  (Right <$> lift (extractMetas normalization))
    `catchTCErr` (fmap Left . lift . extractError)

extractMetas :: Rewrite -> TCM MetasReport
extractMetas normalization = do
  -- The body of @interpret (Cmd_metas norm)@ (InteractionTop.hs:508-510) is a
  -- @getGoals'@ query plus a @getWarningsAndNonFatalErrors@ query, handed to
  -- @display_info@ as @Info_AllGoalsWarnings@. We do the two queries and then
  -- render the payload. See @extractMetas@.
  --
  -- We avoid the implicit load performed by @runInteraction@, leaving that to
  -- the tool layer. It's also worth noting that we don't collect interaction
  -- point data, since @updateInteractionPointsAfter Cmd_metas{}@ evaluates to
  -- false (InteractionTop.hs:430).

  -- @typesOfVisibleMetas@ uses @getInteractionIdsAndMetas@, which obtains the
  -- goals in creation order. We sort by position instead, which is what
  -- @sortInteractionPoints@ does for @theCurrentFile`@s interaction points
  -- (InteractionTop.hs:1046).
  goals <-
    sortOn (positionOffset . spanStart . goalSpan)
      <$> (typesOfVisibleMetas normalization >>= traverse extractGoal)
  -- Note: @interpret Cmd_metas@ uses @(max Simplified norm)@ for the hidden
  -- metavariable normalization, and @AsIs <= Simplified@ by the @deriving@
  -- instance of @Ord@ for @Rewrite@.
  hiddenMetavariables <-
    ( typesOfHiddenMetas (max Simplified normalization)
        >>= traverse extractHiddenMetavariable
    )
  WarningsAndNonFatalErrors warnings nonFatalErrors <-
    getWarningsAndNonFatalErrors
  -- Both frontends run the warnings and non-fatal error sets through
  -- @filterTCWarnings@ before rendering them. Emacs uses @prettyTCWarnings'@
  -- (Pretty/Warning.hs:770), while the JSON frontend explicitly
  -- (JSONTop.hs:305-311).
  warnings' <- filterTCWarnings warnings >>= traverse extractWarning
  nonFatalErrors' <-
    filterTCWarnings nonFatalErrors >>= traverse extractNonFatalError
  pure $ MetasReport goals hiddenMetavariables warnings' nonFatalErrors'

-- Goals and hidden metavariables use only two of @OutputConstraint`@s
-- constructors. The goals response list is built exclusively by @typeOfMetaMI@
-- (BasicOps.hs:889-921), which does cases on @Judgement`@s two
-- constructors. `HasType` becomes @OfType@ and @IsSort@ becomes @JustSort@. The
-- remaining @OutputConstraint@ constructors are used when reifying constraints
-- (@Cmd_constraints@, the @Cmd_goal_type_context*@ family of commands), never
-- for goals.
extractGoal :: OutputConstraint Expr InteractionId -> TCM Goal
extractGoal constraint = do
  -- The relevant Agda code for goal extraction is @showGoals@
  -- (Interaction/BasicOps.hs:830-843) for the Emacs frontend and @encodeTCM@
  -- for the JSON frontend (JSONTop.hs:305-310).
  (goalId, goalShape) <- extractVisibleMetavariable constraint
  -- We claim that after a load, @getInteractionRange@ won't fail and that the
  -- range won't be @NoRange@. If this is wrong, our mental model of Agda needs
  -- to be updated.
  goalSpan <-
    getInteractionRange goalId
      >>= maybe (throwInteractionPointNoRange goalId) pure . rangeSpan
  pure $ Goal goalId goalSpan goalShape
 where
  throwInteractionPointNoRange :: InteractionId -> TCM a
  throwInteractionPointNoRange pointId = do
    points <- getInteractionPoints
    ranges <- traverse (fmap (Text.pack . prettyShow) . getInteractionRange) points
    liftIO $
      throwIO $
        InteractionPointNoRangeBug $
          InteractionPointNoRange pointId $
            zip points ranges

extractVisibleMetavariable ::
  OutputConstraint Expr InteractionId -> TCM (InteractionId, GoalShape)
extractVisibleMetavariable (OfType pointId ty) =
  -- Both frontends render under @withInteractionId@ with the constraint's id,
  -- although they both pass through @OutputForm@ which seems pointless. Anyway,
  -- the Emacs frontend renders the whole constraint as one string, and we want
  -- to render to a @Goal@, which has id, span, and type as separate fields. The
  -- JSON frontend does this too, but instead of the @prettyTCM@ call it uses,
  -- we render the type with @prettyATop@.
  --
  -- @prettyTCM@ on an abstract expression is @abstractToConcrete_@, which
  -- parenthesizes according to the precedence of the ambient scope. The
  -- difference is observable: for @apply {!!}@ with goal type @Nat -> Nat@, the
  -- JSON frontend prints @(Nat -> Nat)@ (with parentheses), while @prettyATop@
  -- prints @Nat -> Nat@ (without parentheses), matching the Emacs display.
  (pointId,)
    <$> ( GoalOfType . Text.pack . render
            <$> withInteractionId pointId (prettyATop ty)
        )
extractVisibleMetavariable (JustSort pointId) = pure $ (pointId, GoalSort)
extractVisibleMetavariable constraint =
  prettyATop constraint
    >>= liftIO
      . throwIO
      . UnexpectedGoalConstraintBug
      . UnexpectedGoalConstraint
      . Text.pack
      . render

extractHiddenMetavariable ::
  OutputConstraint Expr NamedMeta -> TCM HiddenMetavariable
extractHiddenMetavariable constraint =
  case constraint of
    OfType metavariable ty ->
      -- The same reasoning from the visible metavariable case in
      -- @extractVisibleMetavariable@ applies here
      (withMetaId (nmid metavariable) $ prettyATop ty)
        >>= toHiddenMetavariable metavariable . GoalOfType . Text.pack . render
    JustSort metavariable ->
      toHiddenMetavariable metavariable GoalSort
    _ ->
      prettyATop constraint
        >>= liftIO
          . throwIO
          . UnexpectedGoalConstraintBug
          . UnexpectedGoalConstraint
          . Text.pack
          . render
 where
  toHiddenMetavariable :: NamedMeta -> GoalShape -> TCM HiddenMetavariable
  toHiddenMetavariable metavariable shape = do
    name <-
      Text.pack . render
        -- The same as @showA'@ in @showGoals@ (Interaction/BasicOps.hs:838-843)
        <$> (withMetaId (nmid metavariable) $ prettyATop metavariable)
    maybeSpan <- rangeSpan <$> getMetaRange (nmid metavariable)
    pure $ HiddenMetavariable name maybeSpan shape

data MetasBug
  = InteractionPointNoRangeBug InteractionPointNoRange
  | UnexpectedGoalConstraintBug UnexpectedGoalConstraint
  deriving (Show)

data InteractionPointNoRange
  = InteractionPointNoRange InteractionId [(InteractionId, Text)]
  deriving (Show)

data UnexpectedGoalConstraint = UnexpectedGoalConstraint Text
  deriving (Show)

instance Exception MetasBug
