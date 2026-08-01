module AgdaMCP.Interaction.Extract (
  toGiveAction,
  classifyInteractionError,
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

import AgdaMCP.Interaction.Model (
  Error (..),
  GiveAction (..),
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
