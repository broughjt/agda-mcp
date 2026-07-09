{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Common (
  AgdaError (..),
  NonFatalError (..),
  Warning (..),
  failedTail,
  locatedWarnings,
  renderAgdaError,
  resolveError,
  runCommand,
  section,
) where

import Control.Exception (throwIO)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text

import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  Info_Error_boot (..),
  RemoveTokenBasedHighlighting (..),
  Response,
  Response_boot (..),
  Status (..),
 )
import Agda.Syntax.Common.Pretty (render)
import Agda.Syntax.Position (getRange)
import Agda.TypeChecking.Errors (getAllWarningsOfTCErr, renderError)
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (TCErr, TCWarning (tcWarningRange))
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.TypeChecking.Pretty.Warning (filterTCWarnings)
import Agda.Utils.FileName (AbsolutePath)

import AgdaMCP.Position (Span, fileSpan)
import AgdaMCP.Worker (
  Command,
  Worker,
  sendCommand,
 )

-- A `Failure` is a bug in agda-mcp, not a runtime exception we should
-- recover. We throw it here at the tool-handler boundary and deliberately catch
-- it nowhere. This causes the process to die and the dump the error to stderr.
runCommand :: Worker -> Command r -> IO r
runCommand worker command =
  sendCommand worker command >>= either throwIO pure

-- A failed Agda command, consisting of the rendered error text, a span in the
-- loaded file (if indeed the error occurred there, `Nothing` otherwise), and a
-- list of warnings.
data AgdaError = AgdaError
  { agdaErrorMessage :: Text
  , agdaErrorSpan :: Maybe Span
  , agdaErrorWarnings :: [Warning]
  }
  deriving (Show)

newtype Warning = Warning (Maybe Span, Text)
  deriving (Show)

newtype NonFatalError = NonFatalError (Maybe Span, Text)
  deriving (Show)

resolveError :: AbsolutePath -> TCErr -> TCM AgdaError
resolveError path err =
  AgdaError
    <$> (Text.pack <$> renderError err)
    <*> pure (fileSpan path (getRange err))
    <*> (map Warning <$> (getAllWarningsOfTCErr err >>= locatedWarnings path))

-- Apply Agda's own warning filtering (removes unsolved-constraint warnings in
-- the case that there are no "interesting" constraints), then pair each
-- rendered warning with its span.
locatedWarnings :: AbsolutePath -> Set TCWarning -> TCM [(Maybe Span, Text)]
locatedWarnings path warnings =
  filterTCWarnings warnings >>= traverse locate
 where
  locate warning =
    ((,) (fileSpan path (tcWarningRange warning)) . Text.pack . render)
      <$> prettyTCM warning

-- The error message followed by a warnings section, as lines. Callers prepend
-- their own header. The span is not rendered: the message already carries
-- Agda's own location text when the error has one.
renderAgdaError :: AgdaError -> [Text]
renderAgdaError (AgdaError message _ warnings) =
  message : section "Warnings:" [w | Warning (_, w) <- warnings]

-- A titled block of lines, omitted entirely when there are no items.
section :: Text -> [Text] -> [Text]
section _ [] = []
section title items = "" : title : items

-- The error tail shared by the load and give grammars (handleErr,
-- InteractionTop.hs:216-242). `handleErr` only ever wraps `Info_GenericError`
-- (:236); the other `Info_Error` constructors are unreachable from our
-- commands (`Info_CompilationError` only from Cmd_compile :491,
-- `Info_Highlighting{Parse,ScopeCheck}Error` only from Cmd_highlight
-- :631-633), so they fall through to `Nothing` and surface as protocol
-- violations in the callers.
failedTail :: (TCErr -> a) -> [Response] -> Maybe a
failedTail wrap (Resp_DisplayInfo (Info_Error (Info_GenericError e)) : rest) = case rest of
  [ Resp_JumpToError {}
    , Resp_HighlightingInfo _ KeepHighlighting _ _
    , Resp_Status status
    ]
      | not (sChecked status) -> Just (wrap e)
  [Resp_HighlightingInfo _ KeepHighlighting _ _, Resp_Status status]
    | not (sChecked status) -> Just (wrap e)
  _ -> Nothing
failedTail _ _ = Nothing
