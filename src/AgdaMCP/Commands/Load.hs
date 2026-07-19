module AgdaMCP.Commands.Load (Request (..), Response (..), load) where

import Agda.Interaction.Base (
  CommandState (..),
  OutputConstraint_boot (..),
  Rewrite (..),
 )
import Agda.Interaction.BasicOps (
  getWarningsAndNonFatalErrors,
  typesOfHiddenMetas,
  typesOfVisibleMetas,
 )
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.Imports (Mode (..))
import Agda.Interaction.InteractionTop (cmd_load')
import Agda.Interaction.Output (OutputConstraint)
import Agda.Syntax.Abstract (Expr)
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Common.Pretty (prettyShow, render)
import Agda.TypeChecking.Monad (
  NamedMeta,
  TCErr (..),
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
import AgdaMCP.Model (
  Error,
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  NonFatalError,
  Warning,
  extractError,
  extractNonFatalError,
  extractWarning,
 )
import AgdaMCP.Position (rangeSpan)
import AgdaMCP.Session (catchTCErr)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, lift, modify)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text qualified as Text

-- `Cmd_load` takes a path to load and a list of command-line arguments to apply.
data Request = Request {requestPath :: FilePath, requestArguments :: [String]}

-- To the best of my understanding, there are three paths the execution of a `Cmd_load` can take: success, failure, and stale. Besides the interaction layer's general code described in the Main.hs comment, this execution amounts to the body of the `Cmd_load` case of `interpret`, which consists of a call to `cmd_load'` followed by a callback which runs `interpret Cmd_metas`. The first part of `cmd_load'` emits a prelude of responses which is shared by all three paths:
--
-- - `Resp_Status` (with sChecked false)
-- - `Resp_ClearRunningInfo`
-- - `Resp_ClearHighlighting NotOnlyTokenBased`
-- - Zero or more `Resp_RunningInfo` unless we turn verbosity to v0
--
-- Then `cmd_load'` parses the passed command-line arguments and calls `typeCheckMain`, both of which are fallible. `cmd_load'` snapshots the file modification times before and after this block. If they match, then the following commands are emitted:
--
-- - `Resp_Status`
-- - `Resp_DisplayInfo (Info_AllGoalsWarnings)`
-- - `Resp_InteractionPoints`
--
-- These three are emitted as part of the interpretation of `Cmd_metas` and in the `when` block of `runInteraction` for handling commands which modify the interaction points. The data in `Info_AllGoalsWarnings` is the payload we're interested in. It contains a list of visible metavariables (goals the user can interact with), hidden metavariables (an unsolved `_` for example), and a list of warnings and non-fatal errors. We don't collect this information from the `Resp_DisplayInfo` response since we're intentionally ignoring emitted responses. Instead we query this information from Agda state, being careful to do it in the same manner as the relevant Agda code. See comments in some of the `extract*` helpers below for details.
--
-- On the other hand, if the file modification times do not match, the current file and interaction point state is not updated. The responses for the success case are still emitted (except for `Resp_InteractionPoints`, which fails the file-is-still-current check in `runInteraction`), but they won't contain very much useful information. To detect this path we read back Agda's decision instead of repeating the check. See the comment in `load` for details.
--
-- Finally, if an error occurs anywhere in the bodies of `cmd_load'` or `interpret Cmd_metas`, it is thrown as a `TCErr` exception and caught in `handleCommand`. See the discussion in app/Main.hs about this function, but the TL;DR is that I'm pretty confident that it suffices to just `catchTCErr` locally in our wrapper for load. The one thing we have to look after is the branch in `runInteraction`'s `onFail` (InteractionTop.hs:285-286), which makes sure to set `theCurrentFile` to `Nothing` on top of the state resets performed by the `MonadError` implementations for `CommandM` and `TCM`. Also see the comment in `load` for this.
data Response
  = -- The success path: the file type-checked and Agda updated the interaction
    -- state.
    ResponseOk [Goal] [HiddenMetavariable] [Warning] [NonFatalError]
  | -- The failure path: a `TCErr` was thrown in `cmd_load'`; state is rolled
    -- back as described in app/Main.hs.
    ResponseError Error
  | -- The stale path: the file changed on disk while it was being type checked,
    -- so Agda discarded the interaction state.
    ResponseStale

load :: Request -> CommandM Response
load (Request path arguments) =
  ( do
      cmd_load' path arguments True TypeCheck $ const $ pure ()
      -- `cmd_load'` clears `theCurrentFile` as its first action and resets it
      -- only inside the `when (t == t')` block, so after a non-throwing return
      -- `theCurrentFile` is `Just` exactly when the fresh path was
      -- taken. `isJust` therefore distinguishes the two with no race against
      -- further disk changes.
      currentFile <- gets theCurrentFile
      if isJust currentFile
        then
          lift extractResponseOk
        else
          pure ResponseStale
  )
    `catchTCErr` handler
 where
  handler :: TCErr -> CommandM Response
  handler e = do
    -- Even though `cmd_load'` clears `theCurrentFile` during its setup
    -- (:851-853), the automatic state rollback in the `StateT` and `TCM`
    -- `MonadError` instances would restore the *previously* loaded file when
    -- the load throws. The `onFail` clause sets it back to `Nothing` in
    -- `runInteraction`. We match this behavior here.
    modify $ \state -> state {theCurrentFile = Nothing}
    fmap ResponseError $ lift $ extractError e

extractResponseOk :: TCM Response
extractResponseOk = do
  goals <- typesOfVisibleMetas AsIs >>= traverse extractGoal
  -- Note: `interpret Cmd_metas` uses `(max Simplified norm)` for the hidden
  -- metavariable normalization, and `AsIs <= Simplified` by the `deriving`
  -- instance of `Ord` for `Rewrite`.
  hiddenMetavariables <-
    typesOfHiddenMetas Simplified >>= traverse extractHiddenMetavariable
  WarningsAndNonFatalErrors warnings nonFatalErrors <-
    getWarningsAndNonFatalErrors
  warnings' <- traverse extractWarning $ Set.toList warnings
  nonFatalErrors' <- traverse extractNonFatalError $ Set.toList nonFatalErrors
  pure $ ResponseOk goals hiddenMetavariables warnings' nonFatalErrors'

extractGoal :: OutputConstraint Expr InteractionId -> TCM Goal
extractGoal constraint = do
  -- The relevant Agda code for goal extraction is `showGoals`
  -- (Interaction/BasicOps.hs:830-843) for the Emacs frontend and `encodeTCM`
  -- for the JSON frontend (JSONTop.hs:305-310).
  (goalId, goalShape) <- extractVisibleMetavariable constraint
  -- We claim that after a load, `getInteractionRange` won't fail and that the
  -- range won't be `NoRange`. If this is wrong, our mental model of Agda needs
  -- to be updated.
  goalSpan <-
    getInteractionRange goalId
      >>= maybe (throwInteractionPointNoRange goalId) pure . rangeSpan
  pure $ Goal goalId goalSpan goalShape
 where
  throwInteractionPointNoRange :: InteractionId -> TCM a
  throwInteractionPointNoRange pointId = do
    points <- getInteractionPoints
    ranges <- traverse (fmap prettyShow . getInteractionRange) points
    liftIO $
      throwIO $
        InteractionPointNoRangeBug $
          InteractionPointNoRange pointId $
            zip points ranges

extractVisibleMetavariable ::
  OutputConstraint Expr InteractionId -> TCM (InteractionId, GoalShape)
extractVisibleMetavariable (OfType pointId ty) =
  -- Both frontends render under `withInteractionId` with the constraint's id,
  -- although they both pass through `OutputForm` which seems pointless. Anyway,
  -- the Emacs frontend renders the whole constraint as one string, and we want
  -- to render to a `Goal`, which has id, span, and type as separate fields. The
  -- JSON frontend does this too, but instead of the `prettyTCM` call it uses,
  -- we render the type with `prettyATop`.
  --
  -- `prettyTCM` on an abstract expression is `abstractToConcrete_`, which
  -- parenthesizes according to the precedence of the ambient scope. The
  -- difference is observable: for `apply {!!}` with goal type `Nat -> Nat`, the
  -- JSON frontend prints `(Nat -> Nat)` (with parentheses), while `prettyATop`
  -- prints `Nat -> Nat` (without parentheses), matching the Emacs display.
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
      . render

extractHiddenMetavariable ::
  OutputConstraint Expr NamedMeta -> TCM HiddenMetavariable
extractHiddenMetavariable constraint =
  case constraint of
    OfType metavariable ty ->
      -- The same reasoning from the visible metavariable case in
      -- `extractVisibleMetavariable` applies here
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
          . render
 where
  toHiddenMetavariable :: NamedMeta -> GoalShape -> TCM HiddenMetavariable
  toHiddenMetavariable metavariable shape = do
    name <-
      Text.pack . render
        -- The same as `showA'` in `showGoals`
        -- (Interaction/BasicOps.hs:838-843)
        <$> (withMetaId (nmid metavariable) $ prettyATop metavariable)
    maybeSpan <- rangeSpan <$> getMetaRange (nmid metavariable)
    pure $ HiddenMetavariable name maybeSpan shape

data LoadBug
  = InteractionPointNoRangeBug InteractionPointNoRange
  | UnexpectedGoalConstraintBug UnexpectedGoalConstraint
  deriving (Show)

data InteractionPointNoRange
  = InteractionPointNoRange InteractionId [(InteractionId, String)]
  deriving (Show)

data UnexpectedGoalConstraint = UnexpectedGoalConstraint String
  deriving (Show)

instance Exception LoadBug
