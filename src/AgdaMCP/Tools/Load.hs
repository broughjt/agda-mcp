{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Load (
  loadTool,
  Request (..),
  Response (..),
  LoadReport (..),
  load,
  withEditableLoad,
  renderResponse,
) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Syntax.Common (InteractionId (..))
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Data.Aeson (FromJSON (..), object, withObject, (.:), (.=))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )

import AgdaMCP.Interaction (
  ContextEntry,
  Error (..),
  Goal (..),
  GoalError,
  HiddenMetavariable (..),
  MetasReport (..),
  NonFatalError (..),
  Warning,
 )
import AgdaMCP.Interaction.Context qualified as Context
import AgdaMCP.Interaction.Load qualified as Interaction.Load
import AgdaMCP.Tools.LoadId (
  CurrentLoad (..),
  LoadGeneration (..),
  LoadId (..),
  LoadIdRefusal,
  renderLoadId,
  requireCurrentLoad,
 )
import AgdaMCP.Tools.MCP (textToolHandle)
import AgdaMCP.Tools.Render (
  blocks,
  indent,
  renderContextEntry,
  renderShape,
  renderSpan,
  renderWarning,
  section,
 )
import AgdaMCP.Tools.State (ToolM, ToolState (..), liftInteraction)

loadTool :: ToolHandler
loadTool =
  toolHandler
    "load"
    (Just loadDescription)
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [
                ( "path"
                , object
                    [ "type" .= ("string" :: Text)
                    , "description" .= pathDescription
                    ]
                )
              ]
        )
        (Just ["path"])
    )
    (textToolHandle load $ Right . renderResponse)
 where
  loadDescription :: Text
  loadDescription =
    "Load and type-check an Agda source file. On success, reports open goals (each with their local context), unsolved hidden metavariables, warnings, and non-fatal errors. On failure, reports the error message and associated warnings. Agda keeps only one file loaded at a time: each load replaces the one before it and makes fresh goal ID assignments. A successful load returns a load ID for the state it produced. Other tools require that load ID to confirm the request refers to that state, and refuse an earlier one rather than misread it."

  pathDescription :: Text
  pathDescription =
    "Path to an Agda source file (.agda, but also literate formats such as .lagda.md, .lagda.tex, .lagda.typ, etc). Relative paths are resolved against the server process's working directory, so prefer an absolute path when that directory may be ambiguous."

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

{- | Validate the load id, perform an action against the file that load left
behind, then reload it.
-}
withEditableLoad ::
  LoadId ->
  (CurrentLoad -> ToolM outcome) ->
  ToolM (Either LoadIdRefusal (outcome, Response))
withEditableLoad loadId handle =
  gets toolLoadGeneration
    >>= either (pure . Left) run . flip requireCurrentLoad loadId
 where
  run current =
    fmap Right $
      (,)
        <$> handle current
        <*> load Request {loadRequestPath = currentLoadPath current}

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

renderHiddenMetavariable :: HiddenMetavariable -> Text
renderHiddenMetavariable metavariable =
  indent $
    hiddenMetavariableName metavariable
      <> maybe "" ((" at " <>) . renderSpan) (hiddenMetavariableSpan metavariable)
      <> " : "
      <> renderShape (hiddenMetavariableShape metavariable)

-- Like warnings, the structural location beside the message is not rendered:
-- the message embeds its own location.
renderNonFatalError :: NonFatalError -> Text
renderNonFatalError (NonFatalError (_, message)) = indent message
