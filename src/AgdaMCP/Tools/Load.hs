{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Load (
  loadTool,
  Request (..),
  Response (..),
  LoadReport (..),
  LoadRefusal (..),
  load,
  withEditableLoad,
  requireCheckedLoad,
  renderLoadRefusal,
  encodeLoadRefusal,
  renderResponse,
  loadShape,
  loadRefusals,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  Value,
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction (
  Error (..),
  Goal (..),
  HiddenMetavariable (..),
  MetasReport (..),
  NonFatalError (..),
  Warning,
 )
import AgdaMCP.Interaction.Load qualified as Interaction.Load
import AgdaMCP.Tools.Description (
  Description (..),
  Effect (..),
  Shape (..),
  Variant (..),
  field,
  loadIdRefusals,
  parameter,
  sourceRefusals,
  timeoutField,
 )
import AgdaMCP.Tools.Json (
  encodeError,
  encodeGoalId,
  encodeHiddenMetavariable,
  encodeLoadIdRefusal,
  encodeNonFatalError,
  encodeShape,
  encodeSourceRefusal,
  encodeSpan,
  encodeWarning,
  kinded,
 )
import AgdaMCP.Tools.LoadId (
  CurrentLoad (..),
  LoadGeneration (..),
  LoadId (..),
  LoadIdRefusal,
  renderLoadId,
  renderLoadIdRefusal,
  requireCurrentLoad,
 )
import AgdaMCP.Tools.Render (
  blocks,
  indent,
  renderGoalId,
  renderShape,
  renderSourceRefusal,
  renderSpan,
  renderWarning,
  section,
 )
import AgdaMCP.Tools.Source (SourceRefusal, readChecked)
import AgdaMCP.Tools.State (ToolM, ToolState (..), liftInteraction)
import AgdaMCP.Tools.Tool (
  Budget (..),
  Tool (..),
  loadBudget,
  toolHandle,
 )

loadTool :: Tool
loadTool =
  Tool
    { toolDescription = loadDescription
    , toolCall = toolHandle loadBudget load $ Right . renderResponse
    }

loadDescription :: Description
loadDescription =
  Description
    { descriptionName = "load"
    , descriptionSummary =
        "Load and type-check an Agda source file. On success, reports open goals (each with their local context), unsolved hidden metavariables, warnings, and non-fatal errors. On failure, reports the error message and associated warnings. Agda keeps only one file loaded at a time: each load replaces the one before it and makes fresh goal ID assignments. A successful load returns a load ID for the state it produced. Other tools require that load ID to confirm the request refers to that state, and refuse an earlier one rather than misread it."
    , descriptionParameters =
        [ parameter
            "path"
            "Path to an Agda source file (.agda, but also literate formats such as .lagda.md, .lagda.tex, .lagda.typ, etc). Relative paths are resolved against the server process's working directory, so prefer an absolute path when that directory may be ambiguous."
            ShapeString
        , timeoutField (budgetDefault loadBudget)
        ]
    , descriptionAnswers = loadAnswers
    , descriptionRefusals = []
    , descriptionEffect = Loads
    }

loadAnswers :: [Variant]
loadAnswers =
  [ Variant
      "loaded"
      "the file type-checked; its goals, hidden metavariables, warnings and non-fatal errors follow"
      [ field "load_id" ShapeString
      , field "path" ShapeString
      , field "goals" (ShapeArray (ShapeNamed "GOAL"))
      , field "hidden_metavariables" (ShapeArray (ShapeNamed "HIDDEN"))
      , field "warnings" (ShapeArray (ShapeNamed "WARNING"))
      , field "non_fatal_errors" (ShapeArray (ShapeNamed "WARNING"))
      ]
  , Variant "failed" "Agda rejected the file" [field "error" (ShapeNamed "ERROR")]
  , Variant "file_changed" "the file changed on disk while it was being checked" []
  ]

{- | The load answers as the nested sum give and case-split report their
reload under.
-}
loadShape :: Shape
loadShape = ShapeSum "LOAD" loadAnswers

{- | The refusals of a tool that requires a checked load: the load ID's, then
the source's, in the order `requireCheckedLoad` makes them.
-}
loadRefusals :: [Variant]
loadRefusals = loadIdRefusals <> sourceRefusals

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
  , loadReportGoals :: [Goal]
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
      loadId <- issueLoadId file
      pure $
        ResponseOk
          LoadReport
            { loadReportId = loadId
            , loadReportPath = Interaction.Load.loadedFilePath file
            , loadReportGoals = metasReportGoals metasReport
            , loadReportHiddenMetavariables = metasReportHiddenMetavariables metasReport
            , loadReportWarnings = metasReportWarnings metasReport
            , loadReportNonFatalErrors = metasReportNonFatalErrors metasReport
            }
    Interaction.Load.ResponseError e -> ResponseError e <$ clearCurrentLoad
    Interaction.Load.ResponseStale -> ResponseStale <$ clearCurrentLoad

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

data LoadRefusal
  = RefusedLoadId LoadIdRefusal
  | RefusedSource SourceRefusal
  deriving (Eq, Show)

requireCheckedLoad :: LoadId -> ToolM (Either LoadRefusal CurrentLoad)
requireCheckedLoad loadId = do
  generation <- gets toolLoadGeneration
  case requireCurrentLoad generation loadId of
    Left refusal -> pure $ Left $ RefusedLoadId refusal
    Right current -> do
      checked <-
        liftIO $
          readChecked (currentLoadPath current) (currentLoadSourceHash current)
      pure $ either (Left . RefusedSource) (const $ Right current) checked

renderLoadRefusal :: LoadRefusal -> Text
renderLoadRefusal (RefusedLoadId refusal) = renderLoadIdRefusal refusal
renderLoadRefusal (RefusedSource refusal) = renderSourceRefusal refusal

issueLoadId :: Interaction.Load.LoadedFile -> ToolM LoadId
issueLoadId file = do
  generation <- gets toolLoadGeneration
  let issued = 1 + loadsIssued generation
  modify $ \state ->
    state
      { toolLoadGeneration =
          generation
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
  pure $ LoadId (sessionNonce generation) issued

clearCurrentLoad :: ToolM ()
clearCurrentLoad = modify $ \state ->
  state
    { toolLoadGeneration =
        (toolLoadGeneration state) {currentLoad = Nothing}
    }

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "load arguments" $ \o -> Request <$> o .: "path"

-- Response rendering

renderResponse :: Response -> Text
renderResponse (ResponseOk report) =
  blocks $
    [ Text.intercalate
        "\n"
        [ verb <> countGoals (length goals)
        , "Load ID: " <> renderLoadId (loadReportId report)
        , "File: " <> Text.pack (loadReportPath report)
        ]
    ]
      <> [Text.intercalate "\n" (map renderGoal goals) | not (null goals)]
      <> section
        "Unsolved metavariables:"
        (map renderHiddenMetavariable $ loadReportHiddenMetavariables report)
      <> section "Warnings:" (map renderWarning $ loadReportWarnings report)
      <> section
        "Non-fatal errors:"
        (map renderNonFatalError $ loadReportNonFatalErrors report)
 where
  goals = loadReportGoals report
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

renderGoal :: Goal -> Text
renderGoal goal =
  renderGoalId (goalId goal)
    <> " at "
    <> renderSpan (goalSpan goal)
    <> " : "
    <> renderShape (goalShape goal)

renderHiddenMetavariable :: HiddenMetavariable -> Text
renderHiddenMetavariable metavariable =
  indent $
    hiddenMetavariableName metavariable
      <> maybe "" ((" at " <>) . renderSpan) (hiddenMetavariableSpan metavariable)
      <> " : "
      <> renderShape (hiddenMetavariableShape metavariable)

renderNonFatalError :: NonFatalError -> Text
renderNonFatalError (NonFatalError (_, message)) =
  -- Like warnings, the structural location beside the message is not rendered:
  -- the message embeds its own location.
  indent message

instance ToJSON Response where
  toJSON (ResponseOk report) =
    kinded
      "loaded"
      [ "load_id" .= renderLoadId (loadReportId report)
      , "path" .= loadReportPath report
      , "goals" .= map encodeGoal (loadReportGoals report)
      , "hidden_metavariables"
          .= map encodeHiddenMetavariable (loadReportHiddenMetavariables report)
      , "warnings" .= map encodeWarning (loadReportWarnings report)
      , "non_fatal_errors"
          .= map encodeNonFatalError (loadReportNonFatalErrors report)
      ]
  toJSON (ResponseError e) = kinded "failed" ["error" .= encodeError e]
  toJSON ResponseStale = kinded "file_changed" []

encodeGoal :: Goal -> Value
encodeGoal goal =
  object
    [ "id" .= encodeGoalId (goalId goal)
    , "span" .= encodeSpan (goalSpan goal)
    , "shape" .= encodeShape (goalShape goal)
    ]

encodeLoadRefusal :: LoadRefusal -> Value
encodeLoadRefusal (RefusedLoadId refusal) = encodeLoadIdRefusal refusal
encodeLoadRefusal (RefusedSource refusal) = encodeSourceRefusal refusal
