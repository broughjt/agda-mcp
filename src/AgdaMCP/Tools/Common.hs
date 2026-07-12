{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AgdaMCP.Tools.Common (
  AgdaError (..),
  NonFatalError (..),
  Warning (..),
  failedTail,
  locatedWarnings,
  renderAgdaError,
  resolveError,
  section,
  withSession,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put, runStateT)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  MCPHandlerState,
  MCPHandlerUser,
  MCPServerState (..),
  MCPServerT,
 )
import System.IO.Error (ioeGetErrorString)

import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  Info_Error_boot (..),
  RemoveTokenBasedHighlighting (..),
  Response,
  Response_boot (..),
  Status (..),
 )
import Agda.Syntax.Common.Pretty (render)
import Agda.Syntax.Parser.Monad (ParseError (ReadFileError))
import Agda.Syntax.Position (getRange, rangeFilePath)
import Agda.TypeChecking.Errors (getAllWarningsOfTCErr, renderError)
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (
  TCErr (ParserError),
  TCWarning (tcWarningRange),
 )
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.TypeChecking.Pretty.Warning (filterTCWarnings)
import Agda.Utils.FileName (AbsolutePath, filePath)

import AgdaMCP.Position (Span, fileSpan)
import AgdaMCP.Session (Session, SessionM)

type instance MCPHandlerState = Session
type instance MCPHandlerUser = ()

-- Run a session action against the session stored in the MCP server state,
-- storing the successor session back. The transports layer never run two
-- handlers concurrently (stdio is a serial loop and HTTP runs each handler
-- inside `modifyMVar` over the whole server state), so the get/run/put sequence
-- is atomic.
--
-- A `ProtocolViolation` thrown mid-action (a bug in agda-mcp; see
-- `AgdaMCP.Session`) skips the put and propagates out of the handler, killing
-- the process. We deliberately catch it nowhere.
withSession :: SessionM a -> MCPServerT a
withSession action = do
  -- Side note: I think the shape of `withSession` is an instance of a more
  -- general pattern of doing a stateful computation on a projection of a larger
  -- state. Looking around, I think this may have something to do with Haskell's
  -- lenses and the `zoom` operation.
  state <- get
  (result, session) <- liftIO $ runStateT action (mcp_handler_state state)
  put state {mcp_handler_state = session}
  pure result

-- A failed Agda command, consisting of the rendered error text, a span in the
-- loaded file (if indeed the error occurred there, `Nothing` otherwise), and a
-- list of warnings.
data AgdaError = AgdaError
  { agdaErrorMessage :: Text
  , agdaErrorSpan :: Maybe Span
  , agdaErrorWarnings :: [Warning]
  }
  deriving (Eq, Show)

newtype Warning = Warning (Maybe Span, Text)
  deriving (Eq, Show)

newtype NonFatalError = NonFatalError (Maybe Span, Text)
  deriving (Show)

resolveError :: AbsolutePath -> TCErr -> TCM AgdaError
resolveError path e =
  AgdaError
    <$> message
    <*> pure (fileSpan path $ getRange e)
    <*> (map Warning <$> (getAllWarningsOfTCErr e >>= locatedWarnings path))
 where
  -- Agda's own rendering of `ReadFileError` includes the text of the raw
  -- Haskell exception, which repeats the path and adds `openBinaryFile`
  -- noise. We special-case this and render the reason ourselves.
  message = case e of
    ParserError (ReadFileError file readError) ->
      pure $
        "Cannot read file "
          <> Text.pack (filePath (rangeFilePath file))
          <> ": "
          <> Text.pack (ioeGetErrorString readError)
          <> "."
    _ -> Text.pack <$> renderError e

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

-- Render the error message followed by a warnings section, as a list of
-- lines. The span is not rendered, since the message already carries Agda's own
-- location text when the error has one.
renderAgdaError :: AgdaError -> [Text]
renderAgdaError (AgdaError message _ warnings) =
  message : section "Warnings:" [w | Warning (_, w) <- warnings]
 where
  section :: Text -> [Text] -> [Text]
  section _ [] = []
  section title items = "" : title : items

-- The error tail shared by the load and give grammars (handleErr,
-- InteractionTop.hs:216-242). `handleErr` only ever wraps `Info_GenericError`
-- (:236), and the other `Info_Error` constructors are unreachable from our
-- commands (`Info_CompilationError` only from Cmd_compile :491,
-- `Info_Highlighting{Parse,ScopeCheck}Error` only from Cmd_highlight :631-633),
-- so they fall through to `Nothing` and become protocol violations.
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
