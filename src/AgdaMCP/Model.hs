module AgdaMCP.Model (
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  Error (..),
  Warning (..),
  NonFatalError (..),
  extractError,
  extractWarning,
  extractNonFatalError,
) where

import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Common.Pretty (render)
import Agda.TypeChecking.Monad (TCErr, TCM, TCWarning)
import Agda.TypeChecking.Pretty (prettyTCM)
import Data.Text (Text)

import Agda.Syntax.Position (getRange)
import Agda.TypeChecking.Errors (renderError)
import Agda.TypeChecking.Pretty.Warning (
  filterTCWarnings,
  getAllWarningsOfTCErr,
 )
import AgdaMCP.Position (Span, rangePathSpan)
import Data.Text qualified as Text

-- Goals

data Goal = Goal
  { goalId :: InteractionId
  , goalSpan :: Span
  , goalShape :: GoalShape
  }

-- TODO: Explain `interpret Cmd_metas` calls `getGoals'` which calls
-- `typesOfVisibleMetas` and `typesOfHiddenMetas`. These in turn (transitively)
-- call `typeOfMetaMI`. `typeOfMetaMI` only builds `OutputConstraints` with the
-- `HasType` and `JustSort` constructors, so these are the only
-- `OutputConstraints` that will show up in goals obtained from `Cmd_meta`.

-- Later: Emacs rendering of `Resp_DisplayInfo (Info_AllGoalsWarnings)` calls
-- `showGoals` which calls

-- Goals and hidden metavariables use only two of `OutputConstraint`'s
-- constructors. The goals response list is built exclusively by `typeOfMetaMI`
-- (BasicOps.hs:889-921), which does cases on `Judgement`'s two
-- constructors. `HasType` becomes `OfType` and `IsSort` becomes `JustSort`. The
-- remaining `OutputConstraint` constructors are used when reifying constraints
-- (`Cmd_constraints`, the `Cmd_goal_type_context*` family of commands), never
data GoalShape
  = GoalOfType Text
  | GoalSort

data HiddenMetavariable = HiddenMetavariable
  { hiddenMetavariableName :: Text
  , hiddenMetavariableSpan :: Maybe Span
  , hiddenMetavariableShape :: GoalShape
  }

-- Errors

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
