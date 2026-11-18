module AgdaMCP.Interaction.Extract (
  toGiveAction,
  classifyInteractionError,
  extractError,
  extractWarning,
  extractNonFatalError,
  rangeSpan,
  extractContext,
  extractGoalReport,
  extractMetas,
) where

import Agda.Interaction.Base (OutputConstraint_boot (..), Rewrite (..))
import Agda.Interaction.BasicOps (
  getConstraintsMentioning,
  getIPBoundary,
  getResponseContext,
  getWarningsAndNonFatalErrors,
  typeOfMeta,
  typesOfHiddenMetas,
  typesOfVisibleMetas,
 )
import Agda.Interaction.Output (OutputConstraint)
import Agda.Interaction.Response (GiveResult (..), ResponseContextEntry (..))
import Agda.Syntax.Abstract (Expr)
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common (
  Arg (..),
  InteractionId,
  ModalPolarity (..),
  Modality,
  getCohesion,
  getModalPolarity,
  getQuantity,
  getRelevance,
  inverseComposeRelevance,
  isInstance,
  isRelevant,
  modPolarityAnn,
  moreQuantity,
 )
import Agda.Syntax.Common.Pretty (pretty, prettyShow, render)
import Agda.Syntax.Concrete.Name (NameInScope (..), isInScope)
import Agda.Syntax.Position (
  IntervalWithoutFile,
  Position' (..),
  PositionWithoutFile,
  Range,
  RangeFile (..),
  getRange,
  iEnd,
  iStart,
  posCol,
  posLine,
  rangeFile,
  rangeToInterval,
 )
import Agda.TypeChecking.Errors (renderError, verbalize)
import Agda.TypeChecking.Monad (
  InteractionError (..),
  NamedMeta,
  TCErr (..),
  TCM,
  TCWarning,
  TypeError (..),
  WarningsAndNonFatalErrors (..),
  clValue,
  currentModality,
  getMetaRange,
  lookupInteractionId,
  nmid,
  withInteractionId,
  withMetaId,
 )
import Agda.TypeChecking.Monad.MetaVars (
  getInteractionPoints,
  getInteractionRange,
 )
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.TypeChecking.Pretty.Warning (
  filterTCWarnings,
  getAllWarningsOfTCErr,
 )
import Agda.Utils.FileName (filePath)
import Agda.Utils.Maybe.Strict qualified as Strict
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Model (
  ContextEntry (..),
  Error (..),
  GiveAction (..),
  Goal (..),
  GoalReport (..),
  GoalShape (..),
  HiddenMetavariable (..),
  MetasReport (..),
  NonFatalError (..),
  Position (..),
  Span (..),
  Warning (..),
 )

-- Give

toGiveAction :: GiveResult -> GiveAction
toGiveAction (Give_String s) = GiveComputed $ Text.pack s
toGiveAction Give_Paren = GiveVerbatim True
toGiveAction Give_NoParen = GiveVerbatim False

-- Errors and warnings

extractError :: TCErr -> TCM Error
extractError e =
  Error
    <$> message
    <*> pure spanPath
    <*> warnings
 where
  message :: TCM Text
  message = Text.pack <$> renderError e

  spanPath :: Maybe (FilePath, Span)
  spanPath = rangePathSpan $ getRange e

  warnings :: TCM [Warning]
  warnings =
    getAllWarningsOfTCErr e
      >>= filterTCWarnings
      >>= traverse extractWarning

classifyInteractionError ::
  (InteractionId -> e) -> (Error -> e) -> TCErr -> TCM e
classifyInteractionError unknownId failed e = case e of
  TypeError {tcErrClosErr = closure}
    | InteractionError (NoSuchInteractionPoint badId) <- clValue closure ->
        pure (unknownId badId)
  _ -> failed <$> extractError e

extractWarning :: TCWarning -> TCM Warning
extractWarning = fmap Warning . extractWarning'

extractNonFatalError :: TCWarning -> TCM NonFatalError
extractNonFatalError = fmap NonFatalError . extractWarning'

extractWarning' :: TCWarning -> TCM (Maybe (FilePath, Span), Text)
extractWarning' warning = (pathSpan,) <$> message
 where
  pathSpan :: Maybe (FilePath, Span)
  pathSpan = rangePathSpan $ getRange warning

  message :: TCM Text
  message = Text.pack . render <$> prettyTCM warning

-- Positions and spans

toPosition :: PositionWithoutFile -> Position
toPosition p =
  Position
    { positionOffset = fromIntegral (posPos p) - 1
    , positionLine = fromIntegral (posLine p)
    , positionColumn = fromIntegral (posCol p)
    }

toSpan :: IntervalWithoutFile -> Span
toSpan i =
  Span
    (toPosition (iStart i))
    (toPosition (iEnd i))

rangePath :: Range -> Maybe FilePath
rangePath = fmap (filePath . rangeFilePath) . Strict.toLazy . rangeFile

rangeSpan :: Range -> Maybe Span
rangeSpan = fmap toSpan . rangeToInterval

rangePathSpan :: Range -> Maybe (FilePath, Span)
rangePathSpan r = (,) <$> rangePath r <*> rangeSpan r

-- Context

{- | Extraction mirroring @prettyResponseContext@ (EmacsTop.hs). Also
see the comment above @ContextEntry@.
-}
extractContext :: Rewrite -> InteractionId -> TCM [ContextEntry]
extractContext normalization goalId = withInteractionId goalId $ do
  entries <- getResponseContext normalization goalId
  goalModality <- currentModality
  traverse (extractContextEntry goalModality) entries

extractContextEntry :: Modality -> ResponseContextEntry -> TCM ContextEntry
extractContextEntry
  goalModality
  ( ResponseContextEntry
      originalName
      reifiedName
      (Arg info ty)
      letValue
      reifiedInScope
    ) = do
    typeDoc <- prettyATop ty
    letValueDoc <- traverse prettyATop letValue
    pure
      ContextEntry
        { contextEntryOriginalName = Text.pack $ prettyShow originalName
        , contextEntryReifiedName = Text.pack $ prettyShow reifiedName
        , contextEntryOriginalInScope = isInScope originalName == InScope
        , contextEntryReifiedInScope = reifiedInScope == InScope
        , contextEntryType = Text.pack $ render typeDoc
        , contextEntryLetValue = Text.pack . render <$> letValueDoc
        , -- Exactly as @prettyResponseContext@ does it
          contextEntryIsInstance = isInstance info
        , contextEntryCohesion = cohesion
        , contextEntryPolarity = polarity
        , -- Exactly as @prettyResponseContext@ does it
          contextEntryErased =
            not $ getQuantity info `moreQuantity` getQuantity goalModality
        , contextEntryRelevance = relevance
        }
   where
    -- Emacs renders cohesion with @prettyShow@. The default @Continuous@
    -- @prettyShow@'s as empty (Syntax/Common.hs), which we detect
    -- with @null@ and extract as @Nothing@.
    cohesion :: Maybe Text.Text
    cohesion =
      let c = prettyShow $ getCohesion info
       in if null c then Nothing else Just $ Text.pack c

    polarity :: Maybe Text.Text
    polarity =
      -- Exactly as @prettyResponseContext@ does it
      let p = modPolarityAnn $ getModalPolarity info
       in if p == MixedPolarity
            then Nothing
            else Just $ Text.pack $ verbalize p

    relevance :: Maybe Text.Text
    relevance =
      -- Exactly as @prettyResponseContext@ does it
      let r = getRelevance goalModality `inverseComposeRelevance` getRelevance info
       in if isRelevant r
            then Nothing
            else Just $ Text.pack $ verbalize r

-- Goals

{- | Following the body of @cmd_goal_type_context_and@
(InteractionTop.hs) plus the goal shape both frontends re-query at
render time (EmacsTop.hs, JSONTop.hs). Shared as a helper for the
infer and check wrappers.
-}
extractGoalReport :: Rewrite -> InteractionId -> TCM GoalReport
extractGoalReport normalization goalId = do
  context <- extractContext normalization goalId
  shape <- extractGoalShape normalization goalId
  -- @getIPBoundary@ returns the wanted faces. Both frontends render each face
  -- with @pretty@ (EmacsTop.hs, JSONTop.hs uses @encodePretty =
  -- encodeShow . pretty@). Empty when the goal is non-cubical.
  boundary <-
    map (Text.pack . render . pretty) <$> getIPBoundary normalization goalId
  -- Constraints mentioning this goal's metavariable. Emacs renders these with
  -- @prettyTCM@ (EmacsTop.hs) while JSON uses @pretty@. Apparently the the
  -- two can differ; we follow Emacs.
  constraints <-
    lookupInteractionId goalId
      >>= getConstraintsMentioning normalization
      >>= traverse (fmap (Text.pack . render) . prettyTCM)
  pure $ GoalReport shape boundary context constraints

-- | Obtain the goal shape mirroring @prettyTypeOfMeta@ (EmacsTop.hs)
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
          . GoalBug
          . UnexpectedGoalConstraint
          . Text.pack
          . render

newtype GoalBug = GoalBug UnexpectedGoalConstraint
  deriving (Show)

instance Exception GoalBug

-- Metas

extractMetas :: Rewrite -> TCM MetasReport
extractMetas normalization = do
  -- The body of @interpret (Cmd_metas norm)@ (InteractionTop.hs) is a
  -- @getGoals'@ query plus a @getWarningsAndNonFatalErrors@ query, handed to
  -- @display_info@ as @Info_AllGoalsWarnings@. We do the two queries and then
  -- render the payload. See @extractMetas@.
  --
  -- We avoid the implicit load performed by @runInteraction@, leaving that to
  -- the tool layer. It's also worth noting that we don't collect interaction
  -- point data, since @updateInteractionPointsAfter Cmd_metas{}@ evaluates to
  -- false (InteractionTop.hs).

  -- @typesOfVisibleMetas@ uses @getInteractionIdsAndMetas@, which obtains the
  -- goals in creation order. We sort by position instead, which is what
  -- @sortInteractionPoints@ does for @theCurrentFile`@s interaction points
  -- (InteractionTop.hs).
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
  -- (Pretty/Warning.hs), while the JSON frontend explicitly
  -- (JSONTop.hs).
  warnings' <- filterTCWarnings warnings >>= traverse extractWarning
  nonFatalErrors' <-
    filterTCWarnings nonFatalErrors >>= traverse extractNonFatalError
  pure $ MetasReport goals hiddenMetavariables warnings' nonFatalErrors'

-- Goals and hidden metavariables use only two of @OutputConstraint`@s
-- constructors. The goals response list is built exclusively by @typeOfMetaMI@
-- (BasicOps.hs), which does cases on @Judgement`@s two
-- constructors. `HasType` becomes @OfType@ and @IsSort@ becomes @JustSort@. The
-- remaining @OutputConstraint@ constructors are used when reifying constraints
-- (@Cmd_constraints@, the @Cmd_goal_type_context*@ family of commands), never
-- for goals.
extractGoal :: OutputConstraint Expr InteractionId -> TCM Goal
extractGoal constraint = do
  -- The relevant Agda code for goal extraction is @showGoals@
  -- (Interaction/BasicOps.hs) for the Emacs frontend and @encodeTCM@
  -- for the JSON frontend (JSONTop.hs).
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
        -- The same as @showA'@ in @showGoals@ (Interaction/BasicOps.hs)
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
