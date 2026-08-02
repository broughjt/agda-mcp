{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Load (
  loadTool,
  Request (..),
  Response (..),
  LoadReport (..),
  load,
  renderResponse,
) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Syntax.Common (InteractionId (..))
import Control.Exception (Exception, throwIO)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Data.Aeson (FromJSON (..), object, withObject, (.:), (.=))
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )

import AgdaMCP.Interaction (
  ContextEntry (..),
  Error (..),
  Goal (..),
  GoalError,
  GoalShape (..),
  HiddenMetavariable (..),
  MetasReport (..),
  NonFatalError (..),
  Warning (..),
 )
import AgdaMCP.Interaction.Context qualified as Context
import AgdaMCP.Interaction.Load qualified as Interaction.Load
import AgdaMCP.Tools.LoadId (
  CurrentLoad (..),
  LoadGeneration (..),
  LoadId (..),
  renderLoadId,
 )
import AgdaMCP.Tools.MCP (textToolHandle)
import AgdaMCP.Tools.Render (renderSpan)
import AgdaMCP.Tools.State (ToolM, ToolState (..), liftInteraction)

loadTool :: ToolHandler
loadTool =
  toolHandler
    "load"
    ( Just
        "Load and typecheck an Agda source file. Reports open goals (each with \
        \the local context at the goal), unsolved hidden metavariables, \
        \non-fatal errors, and warnings on success, or the Agda error if \
        \checking fails. Relative paths are resolved against the server \
        \process's working directory; prefer an absolute path when that \
        \directory may be ambiguous."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [
                ( "path"
                , object
                    [ "type" .= ("string" :: Text)
                    , "description"
                        .= ( "Path to an Agda source file (.agda, but also \
                             \literate formats such as .lagda.md, .lagda.tex, \
                             \.lagda.typ, etc). Relative paths are resolved \
                             \against the server process's working directory." ::
                               Text
                           )
                    ]
                )
              ]
        )
        (Just ["path"])
    )
    (textToolHandle load $ Right . renderResponse)

data Request = Request {loadRequestPath :: FilePath}
  deriving (Eq, Show)

data Response
  = ResponseOk LoadReport
  | ResponseError Error
  | ResponseStale
  deriving (Eq, Show)

data LoadReport = LoadReport
  { loadReportId :: LoadId
  , loadReportPath :: FilePath
  , loadReportGoals :: [(Goal, [ContextEntry])]
  , loadReportHiddenMetavariables :: [HiddenMetavariable]
  , loadReportWarnings :: [Warning]
  , loadReportNonFatalErrors :: [NonFatalError]
  }
  deriving (Eq, Show)

-- Business logic

load :: Request -> ToolM Response
load (Request path) = do
  response <-
    liftInteraction $
      Interaction.Load.load
        Interaction.Load.Request
          { Interaction.Load.requestPath = path
          , -- We deliberately prevent agents from setting command-line arguments in load requests.
            Interaction.Load.requestArguments = []
          }
  case response of
    Interaction.Load.ResponseOk file metasReport -> do
      goals <- traverse withContext $ metasReportGoals metasReport
      loadId <- issueLoadId file
      pure $
        ResponseOk
          LoadReport
            { loadReportId = loadId
            , loadReportPath = Interaction.Load.loadedFilePath file
            , loadReportGoals = goals
            , loadReportHiddenMetavariables = metasReportHiddenMetavariables metasReport
            , loadReportWarnings = metasReportWarnings metasReport
            , loadReportNonFatalErrors = metasReportNonFatalErrors metasReport
            }
    Interaction.Load.ResponseError e -> ResponseError e <$ clearCurrentLoad
    Interaction.Load.ResponseStale -> ResponseStale <$ clearCurrentLoad
 where
  -- Agda's load command does not report contexts, so we call the context
  -- wrapper for each reported goal.
  withContext :: Goal -> ToolM (Goal, [ContextEntry])
  withContext goal =
    liftInteraction
      ( Context.context
          Context.Request
            { Context.requestNormalization = AsIs
            , Context.requestGoalId = goalId goal
            }
      )
      >>= either
        (liftIO . throwIO . LoadContextUnavailable (goalId goal))
        (pure . (,) goal)

issueLoadId :: Interaction.Load.LoadedFile -> ToolM LoadId
issueLoadId file = do
  issued <- gets $ (1 +) . loadsIssued . toolLoadGeneration
  modify $ \state ->
    state
      { toolLoadGeneration =
          LoadGeneration
            { loadsIssued = issued
            , currentLoad =
                Just
                  CurrentLoad
                    { currentLoadPath = Interaction.Load.loadedFilePath file
                    , currentLoadSourceHash =
                        Interaction.Load.loadedFileSourceHash file
                    }
            }
      }
  pure $ LoadId issued

clearCurrentLoad :: ToolM ()
clearCurrentLoad = modify $ \state ->
  state
    { toolLoadGeneration =
        (toolLoadGeneration state) {currentLoad = Nothing}
    }

-- Bug: a goal reported by the load that just completed failed to resolve its
-- context. The goal ids come from the same interaction-point table the context
-- query reads, so this means our mental model of Agda is wrong.
data LoadContextUnavailable = LoadContextUnavailable InteractionId GoalError
  deriving (Show)

instance Exception LoadContextUnavailable

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "load arguments" $ \o -> Request <$> o .: "path"

-- Response rendering

renderResponse :: Response -> Text
renderResponse (ResponseOk report) =
  blocks $
    [ Text.intercalate
        "\n"
        [ verb <> countGoals (length $ loadReportGoals report)
        , "Load ID: " <> renderLoadId (loadReportId report)
        , "File: " <> Text.pack (loadReportPath report)
        ]
    ]
      <> map (uncurry renderGoal) (loadReportGoals report)
      <> section
        "Unsolved metavariables:"
        (map renderHiddenMetavariable $ loadReportHiddenMetavariables report)
      <> section "Warnings:" (map renderWarning $ loadReportWarnings report)
      <> section
        "Non-fatal errors:"
        (map renderNonFatalError $ loadReportNonFatalErrors report)
 where
  -- Non-fatal errors change the verb, but metavariable and warning sections do
  -- not alter what "succeeded" means.
  verb
    | null (loadReportNonFatalErrors report) = "Load succeeded: "
    | otherwise = "Load succeeded with errors: "
renderResponse (ResponseError e) =
  blocks $
    ["Load failed:", indent $ errorMessage e]
      <> section "Warnings:" (map renderWarning $ errorWarnings e)
renderResponse ResponseStale =
  "Load did not finish: the file changed on disk while it was being checked. \
  \Load it again."

countGoals :: Int -> Text
countGoals 0 = "no goals"
countGoals 1 = "1 goal"
countGoals n = Text.pack (show n) <> " goals"

renderGoal :: Goal -> [ContextEntry] -> Text
renderGoal goal entries =
  Text.intercalate "\n" $
    ("?" <> Text.pack (show $ interactionId $ goalId goal))
      <> " at "
      <> renderSpan (goalSpan goal)
      -- We reverse context entries so they are listed innermost-first.
      : map (indent . renderContextEntry) (reverse entries)
        <> [indent $ "⊢ " <> renderShape (goalShape goal)]

renderShape :: GoalShape -> Text
renderShape (GoalOfType t) = t
renderShape GoalSort = "Sort"

-- Follows `prettyResponseContext` (EmacsTop.hs:324-373), minus its `align 10`
-- padding. We render cohesion as a prefix, the three-form name rule, one
-- comma-separated attribute group in Agda's order appended after the type, and
-- a let value on its own line.
renderContextEntry :: ContextEntry -> Text
renderContextEntry entry =
  case contextEntryLetValue entry of
    Nothing -> typed
    Just value -> typed <> "\n" <> name <> " = " <> value
 where
  typed = nameWithMaybeCohesion <> " : " <> contextEntryType entry <> attributeGroup
  nameWithMaybeCohesion = maybe name (<> (" " <> name)) $ contextEntryCohesion entry
  name
    | not (contextEntryOriginalInScope entry) = contextEntryReifiedName entry
    | contextEntryOriginalName entry == contextEntryReifiedName entry =
        contextEntryOriginalName entry
    | otherwise =
        contextEntryOriginalName entry <> " = " <> contextEntryReifiedName entry
  attributeGroup
    | null attributes = ""
    | otherwise = " (" <> Text.intercalate ", " attributes <> ")"
  attributes =
    catMaybes
      [ "not in scope" <$ guard (not $ contextEntryReifiedInScope entry)
      , "erased" <$ guard (contextEntryErased entry)
      , contextEntryRelevance entry
      , contextEntryPolarity entry
      , "instance" <$ guard (contextEntryIsInstance entry)
      ]

renderHiddenMetavariable :: HiddenMetavariable -> Text
renderHiddenMetavariable metavariable =
  indent $
    hiddenMetavariableName metavariable
      <> maybe "" ((" at " <>) . renderSpan) (hiddenMetavariableSpan metavariable)
      <> " : "
      <> renderShape (hiddenMetavariableShape metavariable)

-- The structural location beside the message is not rendered: the message
-- embeds its own location, so printing both would print it twice.
renderWarning :: Warning -> Text
renderWarning (Warning (_, message)) = indent message

renderNonFatalError :: NonFatalError -> Text
renderNonFatalError (NonFatalError (_, message)) = indent message

-- Blocks are separated by one blank line.
blocks :: [Text] -> Text
blocks = Text.intercalate "\n\n"

-- A titled section around its already-indented items, or the empty string.
section :: Text -> [Text] -> [Text]
section _ [] = []
section title items = [title <> "\n" <> blocks items]

-- Every line, so multi-line payloads keep their own internal indentation.
indent :: Text -> Text
indent = Text.intercalate "\n" . map ("  " <>) . Text.splitOn "\n"
