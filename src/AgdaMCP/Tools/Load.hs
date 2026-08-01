{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Load (
  loadTool,
  Request (..),
  Response (..),
  LoadReport (..),
  load,
  renderResponse,
) where

import Data.Aeson (FromJSON (..), object, withObject, (.:), (.=))
import Data.Map qualified as Map
import Data.Text (Text)
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )

import AgdaMCP.Interaction (
  ContextEntry,
  Error,
  Goal,
  HiddenMetavariable,
  NonFatalError,
  Warning,
 )
import AgdaMCP.Tools.LoadId (LoadId (..))
import AgdaMCP.Tools.MCP (textToolHandle)
import AgdaMCP.Tools.State (ToolM)

loadTool :: ToolHandler
loadTool =
  toolHandler
    "load"
    ( Just ""
    -- TODO:
    -- "Load and typecheck an Agda source file. Reports open goals (each with \
    -- \the local context at the goal), unsolved hidden metavariables, \
    -- \non-fatal errors, and warnings on success, or the Agda error if \
    -- \checking fails. Relative paths are resolved against the server \
    -- \process's working directory; prefer an absolute path when that \
    -- \directory may be ambiguous."
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
    (textToolHandle load renderResponse)

data Request = Request {loadRequestPath :: FilePath}

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
load = error "un"

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "load arguments" $ \o -> Request <$> o .: "path"

-- Response rendering

renderResponse :: Response -> Text
renderResponse = error "un"
