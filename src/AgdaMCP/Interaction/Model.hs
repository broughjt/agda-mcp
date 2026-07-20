module AgdaMCP.Interaction.Model (
  Goal (..),
  GoalShape (..),
  HiddenMetavariable (..),
  Error (..),
  Warning (..),
  NonFatalError (..),
  Position (..),
  Span (..),
  extractError,
  extractWarning,
  extractNonFatalError,
  rangeSpan,
) where

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
import Agda.TypeChecking.Monad (TCErr, TCM, TCWarning)
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
-- for goals.
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
