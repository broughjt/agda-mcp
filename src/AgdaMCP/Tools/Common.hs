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

import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put, runStateT)
import Data.Foldable (toList)
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

import Agda.Interaction.Options.Warnings (errorWarnings, warningName2String)
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  Info_Error_boot (..),
  RemoveTokenBasedHighlighting (..),
  Response,
  Response_boot (..),
  Status (..),
 )
import Agda.Syntax.Common.Pretty (render)
import Agda.Syntax.Parser.Monad (
  ParseError (ParseError, ReadFileError),
  errInput,
  errMsg,
  errPos,
  errPrevToken,
  errSrcFile,
 )
import Agda.Syntax.Position (
  Range,
  SrcFile,
  getRange,
  rangeFile,
  rangeFilePath,
  rangeToInterval,
 )
import Agda.TypeChecking.Errors (getAllWarningsOfTCErr, renderError)
import Agda.TypeChecking.Errors.Names (typeErrorString)
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (
  Closure (Closure, clEnv, clValue),
  TCEnv (envCall, envRange),
  TCErr (ParserError, TypeError),
  TCWarning (tcWarning, tcWarningRange),
  TypeError (NonFatalErrors),
  warningName,
  withTCState,
 )
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.TypeChecking.Pretty.Warning (filterTCWarnings)
import Agda.Utils.FileName (AbsolutePath, filePath)
import Agda.Utils.Maybe.Strict qualified as Strict
import Agda.Utils.Set1 qualified as Set1

import AgdaMCP.Position (
  Span,
  fileSpan,
  renderPosition,
  renderSpan,
  toPosition,
  toSpan,
 )
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

-- Mirroring `prettyError` (Errors.hs:102-113).
-- TODO: Why
resolveError :: AbsolutePath -> TCErr -> TCM AgdaError
resolveError path e =
  AgdaError
    -- If an error occurs while rendering the message, fall back to Agda's
    -- renderer, which recovers internally.
    <$> (message `catchError` (const $ Text.pack <$> renderError e))
    <*> pure (fileSpan path $ getRange e)
    <*> (map Warning <$> (getAllWarningsOfTCErr e >>= locatedWarnings path))
 where
  -- Bespoke renderings of the error cases whose stock header line bakes in
  -- an absolute path and Agda's dotted range format. Each mirrors its Agda
  -- counterpart with the header rebuilt by `renderLocation`; the bodies are
  -- Agda's own, so ranges *inside* them keep Agda's format (no rendering
  -- hook exists at that depth — accepted residual).
  message = case e of
    -- `NonFatalErrors` bundles warning-level errors (e.g. --safe violations
    -- promoted to an error at the end of checking); Agda renders it as the
    -- bare warnings with no error header (Errors.hs:141-144). Ours reuses
    -- the warning re-headering.
    TypeError _ _ Closure {clValue = NonFatalErrors ws} ->
      Text.intercalate "\n\n" . toList
        <$> traverse (renderTCWarning path) (Set1.toAscList ws)
    -- The general `TypeError` case of `instance PrettyTCM TCErr`
    -- (Errors.hs:150-161): in the state where the error was raised (the
    -- current state may have rolled the failing definitions back), the
    -- header, then the error in its closure's scope, then the call trace.
    TypeError _ s closure -> withTCState (const s) $ do
      body <- Text.pack . render <$> prettyTCM closure
      trace <-
        maybe
          (pure "")
          (fmap (Text.pack . render) . prettyTCM)
          (envCall (clEnv closure))
      pure . joinLines $
        [ header
            (renderLocation path (envRange (clEnv closure)))
            ("error: [" <> Text.pack (typeErrorString (clValue closure)) <> "]")
        , body
        , trace
        ]
    -- Agda's rendering of `ReadFileError` (Parser/Monad.hs:265-268) follows
    -- its human-readable line with the raw Haskell exception, which repeats
    -- the path and adds `openBinaryFile:` noise; render the reason ourselves.
    ParserError (ReadFileError file readError) ->
      pure $
        "Cannot read file "
          <> Text.pack (filePath (rangeFilePath file))
          <> ": "
          <> Text.pack (ioeGetErrorString readError)
          <> "."
    -- `instance Pretty ParseError` (Parser/Monad.hs:246-254): a point
    -- position header, then the message (or, for bare Happy errors, the
    -- surrounding context).
    ParserError parseError@ParseError {} ->
      pure . joinLines $
        [ header
            ( Just $
                qualify path (errSrcFile parseError) $
                  renderPosition (toPosition (errPos parseError))
            )
            "error: [ParseError]"
        , if null (errMsg parseError)
            then
              Text.pack (errPrevToken parseError <> "<ERROR>\n")
                <> Text.pack (take 30 (errInput parseError) <> "...")
            else Text.pack (errMsg parseError)
        ]
    _ -> Text.pack <$> renderError e

  header location kind = maybe kind (\l -> l <> ": " <> kind) location

  joinLines = Text.intercalate "\n" . filter (not . Text.null)

-- A location in the uniform format the server's own text uses (`renderSpan`'s
-- colon form): bare for the loaded file and for file-less ranges (give
-- expression errors), path-qualified for other files, `Nothing` for null
-- ranges.
renderLocation :: AbsolutePath -> Range -> Maybe Text
renderLocation path r =
  qualify path (rangeFile r) . renderSpan . toSpan <$> rangeToInterval r

qualify :: AbsolutePath -> SrcFile -> Text -> Text
qualify path file location = case Strict.toLazy file of
  Just rf
    | rangeFilePath rf /= path ->
        Text.pack (filePath (rangeFilePath rf)) <> ":" <> location
  _ -> location

-- Apply Agda's own warning filtering (removes unsolved-constraint warnings in
-- the case that there are no "interesting" constraints), then pair each
-- rendered warning with its span.
locatedWarnings :: AbsolutePath -> Set TCWarning -> TCM [(Maybe Span, Text)]
locatedWarnings path warnings =
  filterTCWarnings warnings >>= traverse locate
 where
  locate warning =
    (,) (fileSpan path (tcWarningRange warning))
      <$> renderTCWarning path warning

-- `prettyTCM` on a `TCWarning` returns `tcWarningDoc`, a rendering cached
-- when the warning was raised (state and scope have moved on, so it can't
-- be re-rendered here). Its first line is always the header —
-- "<range>: warning: -W[no]Name" or "<range>: error: [Name]", built at
-- Warnings.hs:106-115 with an absolute path and Agda's dotted ranges —
-- and `tcWarningRange` is exactly the header's range, so rebuild that one
-- line in our format and keep the body verbatim. A doc with no newline
-- has an unexpected shape; leave it untouched.
renderTCWarning :: AbsolutePath -> TCWarning -> TCM Text
renderTCWarning path warning = reheader . Text.pack . render <$> prettyTCM warning
 where
  reheader rendered = case Text.breakOn "\n" rendered of
    (_, rest)
      | Text.null rest -> rendered
      | otherwise -> header <> rest

  name = warningName (tcWarning warning)
  kind
    | name `elem` errorWarnings =
        "error: [" <> Text.pack (warningName2String name) <> "]"
    | otherwise = "warning: -W[no]" <> Text.pack (warningName2String name)
  header =
    maybe
      kind
      (\l -> l <> ": " <> kind)
      (renderLocation path (tcWarningRange warning))

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
