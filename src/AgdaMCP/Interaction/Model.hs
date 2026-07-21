module AgdaMCP.Interaction.Model (
  -- Goals
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  ContextEntry (..),
  GoalReport (..),
  -- Give family
  GiveAction (..),
  toGiveAction,
  -- Command error sums
  GiveError (..),
  GoalError (..),
  IntroError (..),
  classifyInteractionError,
  -- Errors/warnings
  Error (..),
  Warning (..),
  NonFatalError (..),
  -- Positions
  Position (..),
  Span (..),
  extractError,
  extractWarning,
  extractNonFatalError,
  rangeSpan,
) where

import Agda.Interaction.Response (GiveResult (..))
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Common.Pretty (render)
import Agda.Syntax.Position (
  IntervalWithoutFile,
  Position' (..),
  PositionWithoutFile,
  Range,
  RangeFile (..),
  getRange,
  iEnd,
  iStart,
  rangeFile,
  rangeToInterval,
 )
import Agda.TypeChecking.Errors (renderError)
import Agda.TypeChecking.Monad (
  InteractionError (..),
  TCErr (..),
  TCM,
  TCWarning,
  TypeError (..),
  clValue,
 )
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.TypeChecking.Pretty.Warning (
  filterTCWarnings,
  getAllWarningsOfTCErr,
 )
import Agda.Utils.FileName (filePath)
import Agda.Utils.Maybe.Strict qualified as Strict
import Data.Text (Text)
import Data.Text qualified as Text

-- Goals

data Goal = Goal
  { goalId :: InteractionId
  , goalSpan :: Span
  , goalShape :: GoalShape
  }
  deriving (Eq, Show)

-- Goals and hidden metavariables use only two of `OutputConstraint`'s
-- constructors. The goals response list is built exclusively by `typeOfMetaMI`
-- (BasicOps.hs:889-921), which does cases on `Judgement`'s two
-- constructors. `HasType` becomes `OfType` and `IsSort` becomes `JustSort`. The
-- remaining `OutputConstraint` constructors are used when reifying constraints
-- (`Cmd_constraints`, the `Cmd_goal_type_context*` family of commands), never
-- for goals.

-- TODO: Explain `interpret Cmd_metas` calls `getGoals'` which calls
-- `typesOfVisibleMetas` and `typesOfHiddenMetas`. These in turn (transitively)
-- call `typeOfMetaMI`. `typeOfMetaMI` only builds `OutputConstraints` with the
-- `HasType` and `JustSort` constructors, so these are the only
-- `OutputConstraints` that will show up in goals obtained from `Cmd_meta`.
data GoalShape
  = GoalOfType Text
  | GoalSort
  deriving (Eq, Show)

data HiddenMetavariable = HiddenMetavariable
  { hiddenMetavariableName :: Text
  , hiddenMetavariableSpan :: Maybe Span
  , hiddenMetavariableShape :: GoalShape
  }
  deriving (Eq, Show)

{-
Relevant Agda code:

- `ResponseContextEntry` Interaction/Response/Base.hs:147-153
- `ArgInfo` Syntax/Common.hs:2464-2472
- the `Info_Context` of `lispifyDisplayInfo` Interaction/EmacsTop.hs:196-198
- `prettyResponseContext` Interaction/EmacsTop.hs:324-373
- `encodeTCM` for `ResponseContextEntry` Interaction/JSONTop.hs:96-102

The Emacs frontend uses the original name, the reified name, and whether the
original name is in scope to render the binding, in one of three forms:

x : Nat        (when original == reified)
x = x₁ : Nat   (when original != reified and the original is in scope)
x₁ : Nat       (otherwise, i.e. when the original is not in scope)

For example, in `f x = λ x → {!!}` the inner x renders in the first form and
the shadowed outer x in the second. The third form arises for
machine-generated binders (e.g. variables generated in a fresh case split).

The reified-in-scope flag is used to append a "not in scope" modifier in
parentheses. The shadowed outer x would render as:

x = x₁ : Nat   (not in scope)

For something like `g = let longname = zero; v = suc zero in {!!}`, Emacs uses
the let-binding value to render:

longname : Nat
longname = zero
v : Nat
v = suc longname

There is a bunch of extra information in the `ArgInfo` record attached to
`respType` in `ResponseContextEntry`. A bunch of that information is not used in
the Emacs `prettyResponseContext` renderer, so we leave it out. Examining
`prettyResponseContext` closely, it renders:

- cohesion
- polarity
- erased
- relevance
- is instance

These are the other fields we include here, even though I don't understand what
they do besides "is instance".
-}
data ContextEntry = ContextEntry
  { contextEntryOriginalName :: Text
  {- ^ The user's name for the binding (`respOrigName` in
  `ResponseContextEntry`).
  -}
  , contextEntryReifiedName :: Text
  {- ^ The user's original name for the binding, unless that name is shadowed in
  the scope of the goal, in which case the first free variant (x₁, x₂, etc.).
  -}
  , contextEntryOriginalInScope :: Bool
  -- ^ Whether the original name is in scope.
  , contextEntryReifiedInScope :: Bool
  {- ^ Whether the reified name is in scope (`respInScope` in
  `ResponseContextEntry`).
  -}
  , contextEntryType :: Text
  {- ^ The type of the binding (at the requested normalization, `respType` in
  `ResponseContextEntry`).
  -}
  , contextEntryLetValue :: Maybe Text
  {- ^ The value when this is a let binding (`respLetValue` in
  `ResponseContextEntry`)
  -}
  , contextEntryIsInstance :: Bool
  -- ^ Whether the variable is considered by instance search.
  , contextEntryCohesion :: Maybe Text
  , contextEntryPolarity :: Maybe Text
  , contextEntryErased :: Bool
  , contextEntryRelevance :: Maybe Text
  -- I don't understand what these last four are for but they are here for
  -- completeness. I think they all arise from some of the more exotic type
  -- theory features Agda supports, but I haven't used any of these features
  -- yet.
  }
  deriving (Eq, Show)

-- The shared payload of the `Cmd_goal_type_context*` family, which is collected
-- in `cmd_goal_type_context_and` (InteractionTop.hs:1061-1067). It bundles the
-- goal type, boundary faces, context, constraints mentioning the goal's
-- metavariable.
--
-- Note the goal type is not part of Agda's `Goal_GoalType` payload. Both
-- frontends re-query it at render time via `typeOfMeta` (`prettyTypeOfMeta`,
-- EmacsTop.hs:378 / JSONTop.hs:395). We carry it explicitly as a `GoalShape`,
-- the same shape load's goal inventory uses.
data GoalReport = GoalReport
  { goalReportShape :: GoalShape
  -- ^ The goal type from `typeOfMeta` at the requested normalization.
  , goalReportBoundary :: [Text]
  -- ^ The boundary faces from `getIPBoundary`. Empty for a non-cubical goal.
  , goalReportContext :: [ContextEntry]
  -- ^ The goal's context from `getResponseContext`.
  , goalReportConstraints :: [Text]
  -- ^ Constraints mentioning this goal's metavariable from `getConstraintsMentioning`.
  }
  deriving (Eq, Show)

-- How a goal/context command can fail (goal, infer, check, context all share
-- this).
data GoalError
  = GoalUnknownId InteractionId
  | GoalFailed Error
  deriving (Eq, Show)

-- Give

-- What to place in the hole once a give/refine/intro/elaborate succeeds. It
-- duplicates `GiveResult` and exists because Agda's `GiveResult`
-- (Response/Base.hs:175-178) has no `Eq` or `Show` instances.
data GiveAction
  = -- Agda kept the user's own expression text. The `Bool` is whether the
    -- expression must be parenthesized. Emitted for give/refine when the given
    -- expression is unchanged and the hole has a real range (`literally` at
    -- InteractionTop.hs:1010).
    GiveVerbatim Bool
  | -- Agda computed a new concrete expression to place (intro, elaborate, or a
    -- refine that introduced metavariables).
    GiveComputed Text
  deriving (Eq, Show)

toGiveAction :: GiveResult -> GiveAction
toGiveAction (Give_String s) = GiveComputed $ Text.pack s
toGiveAction Give_Paren = GiveVerbatim True
toGiveAction Give_NoParen = GiveVerbatim False

-- How a give/refine/elaborate command can fail.
data GiveError
  = -- A bogus interaction id (`withInteractionId`'s lookup failed).
    GiveUnknownId InteractionId
  | -- Any other `TCErr`. For example, a parse error, `CannotGive`,
    -- `CannotRefine`, or an ill-typed expression.
    GiveFailed Error
  deriving (Eq, Show)

-- Ways in which an intro command can fail. This is a superset of `GiveError`,
-- since `introTactic` runs before any give and can conclude that no
-- constructor/lambda applies (`Info_Intro_NotFound`) or that several do
-- (`Info_Intro_ConstructorUnknown`).
data IntroError
  = IntroUnknownId InteractionId
  | IntroFailed Error
  | IntroNotFound
  | IntroAmbiguous [Text]
  deriving (Eq, Show)

-- Generic errors

data Error = Error
  { agdaErrorMessage :: Text
  , agdaErrorPathSpan :: Maybe (FilePath, Span)
  , agdaErrorWarnings :: [Warning]
  }
  deriving (Eq, Show)

newtype Warning = Warning (Maybe (FilePath, Span), Text)
  deriving (Eq, Show)

newtype NonFatalError = NonFatalError (Maybe (FilePath, Span), Text)
  deriving (Eq, Show)

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

-- Helper for classifying missing interaction IDs.
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

-- Positions and Spans

-- A position in a loaded file, consisting of a zero-based offset into the
-- Agda-normalized source text (`posPos` counts code points of the source
-- after Agda's line-ending normalization, so file edits must splice against
-- that same normalized text) and the one-based line/column that Agda
-- prints. Agda's `posPos` is one-based, hence the subtraction in
-- `toPosition`.
data Position = Position
  { positionOffset :: Int
  , positionLine :: Int
  , positionColumn :: Int
  }
  deriving (Eq, Show)

-- A contiguous part of the loaded file with start inclusive and end exclusive.
data Span = Span
  { spanStart :: Position
  , spanEnd :: Position
  }
  deriving (Eq, Show)

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

-- fileSpan :: AbsolutePath -> Agda.Syntax.Position.Range -> Maybe Span
-- fileSpan p r = do
--   rangeFile <- Strict.toLazy $ Agda.Syntax.Position.rangeFile r
--   guard $ Agda.Syntax.Position.rangeFilePath rangeFile == p
--   toSpan <$> Agda.Syntax.Position.rangeToInterval r

-- rangeFile' :: Range' p -> Maybe p
-- rangeFile' NoRange = Nothing
-- rangeFile' (Range p _) = Just p

rangePath :: Range -> Maybe FilePath
rangePath = fmap (filePath . rangeFilePath) . Strict.toLazy . rangeFile

rangeSpan :: Range -> Maybe Span
rangeSpan = fmap toSpan . rangeToInterval

rangePathSpan :: Agda.Syntax.Position.Range -> Maybe (FilePath, Span)
rangePathSpan r = (,) <$> rangePath r <*> rangeSpan r

-- rangeMaybeToFileSpan Agda.Syntax.Position.NoRange = error "unimplemented"
-- rangeMaybeToFileSpan r@(Agda.Syntax.Position.Range a b) =
--   let foo = rangeToInterval r
--    in error "unimplemented"

-- spanText :: Text -> Span -> Text
-- spanText t s =
--   Text.take
--     (spanLength s)
--     (Text.drop (positionOffset (spanStart s)) t)

-- spanLength :: Span -> Int
-- spanLength s = positionOffset (spanEnd s) - positionOffset (spanStart s)

-- renderSpan :: Span -> Text
-- renderSpan s
--   | positionLine start == positionLine end =
--       renderPosition start <> "-" <> Text.pack (show (positionColumn end))
--   | otherwise = renderPosition start <> "-" <> renderPosition end
--  where
--   start = spanStart s
--   end = spanEnd s

-- renderPosition :: Position -> Text
-- renderPosition (Position _ l c) =
--   Text.pack (show l) <> ":" <> Text.pack (show c)
