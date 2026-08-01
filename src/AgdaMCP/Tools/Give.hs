{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Give (
  giveTool,
  Request (..),
  Response (..),
  Action (..),
  Outcome (..),
  Edit (..),
  EditKind (..),
  Refusal (..),
  BatchPosition (..),
  RefusalReason (..),
  give,
  renderResponse,
) where

import MCP.Server (InputSchema (..), ToolHandler, toolHandler)

import Agda.Interaction.Base (Rewrite)
import Agda.Syntax.Common (InteractionId)
import Data.Aeson (FromJSON (..), object, (.=))
import Data.Map qualified as Map
import Data.Text (Text)

import AgdaMCP.Interaction.Model (Error, Span)
import AgdaMCP.Tools.Internal (LoadId, LoadIdRefusal, ToolM, textToolHandle)
import AgdaMCP.Tools.Load qualified as Load

giveTool :: ToolHandler
giveTool =
  toolHandler
    "give"
    ( Just
        -- TODO:
        "Fill one or more goals in an Agda source file. Takes the file `path` and a non-empty list of `gives`, each consisting of a goal interaction ID and an expression. The file must be the currently loaded file. Agda tracks goals for one file at a time, so goal interaction IDs are only valid for the most recently loaded file, and a give against any other file is refused and returns that file's fresh load result to give against instead. Gives are checked in order as an atomic batch: if one is rejected, subsequent gives are skipped and no source edits are written. Before writing, the server verifies that the file on disk still matches the source Agda checked; if it changed, all edits are refused. Every checked outcome is followed by a reload to resync. Interaction IDs may change, so use the goals in the fresh result. Successful gives write Agda’s elaborated, pretty-printed expressions, which may differ from the submitted text. The result reports both when they differ. Relative paths are resolved against the server process’s working directory. Prefer an absolute path when that directory may be ambiguous."
    )
    -- TODO: Out of date
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
              ,
                ( "gives"
                , object
                    [ "type" .= ("array" :: Text)
                    , "description"
                        .= ( "A non-empty, all-or-nothing batch of goals to fill and expressions to fill them with" ::
                               Text
                           )
                    , "items"
                        .= object
                          [ "type" .= ("object" :: Text)
                          , "properties"
                              .= object
                                [ "goal"
                                    .= object
                                      [ "type" .= ("integer" :: Text)
                                      , "description"
                                          .= ("The target goal's interaction ID (`?N`) from a load result" :: Text)
                                      ]
                                , "expression"
                                    .= object
                                      [ "type" .= ("string" :: Text)
                                      , "description"
                                          .= ( "The Agda expression to check and fill the goal with" ::
                                                 Text
                                             )
                                      ]
                                ]
                          , "required" .= (["goal", "expression"] :: [Text])
                          ]
                    ]
                )
              ]
        )
        (Just ["path", "gives"])
    )
    (textToolHandle give renderResponse)

data Request = Request
  { giveRequestLoadId :: LoadId
  , giveRequestItems :: [(InteractionId, Action)]
  }
  deriving (Eq, Show)

data Action
  = ActionGive Text
  | ActionRefine Text
  | ActionElaborate Rewrite Text
  | -- | whether to use a pattern-matching lambda
    ActionIntro Bool
  deriving (Eq, Show)

data Response
  = -- The load id was refused.
    ResponseRefused LoadIdRefusal
  | -- The outcome of the batch of actions, together with the result of a reload
    -- executed afterwards.
    ResponseCompleted Outcome Load.Response
  deriving (Eq, Show)

-- TODO: Revisit when doing give tool error handling
data Outcome
  = OutcomeApplied [Edit]
  | OutcomeRefused Refusal
  | OutcomeFileChanged
  | -- Writing the edits failed. The payload is the rendered `IOException`,
    -- which is the whole value of the refusal (which path, and why).
    OutcomeIOError Text
  deriving (Eq, Show)

data Edit = Edit
  { editGoalId :: InteractionId
  , editSpan :: Span
  , editText :: Text
  , editKind :: EditKind
  }
  deriving (Eq, Show)

data EditKind = EditKindVerbatim | EditKindParentheses | EditKindComputed
  deriving (Eq, Show)

data Refusal = Refusal
  { refusalGoalId :: InteractionId
  , -- TODO:
    refusalSpan :: Maybe Span
  {- ^ The hole being filled. `Nothing` for `RefusedUnknownGoal`, which has no
  interaction point and therefore no range to report.
  -}
  , refusalPosition :: BatchPosition
  , refusalReason :: RefusalReason
  }
  deriving (Eq, Show)

data BatchPosition = BatchPosition {batchIndex :: Int, batchLength :: Int}
  deriving (Eq, Show)

data RefusalReason
  = RefusedUnknownGoal
  | RefusedError Error
  | RefusedIntroNotFound
  | RefusedIntroAmbiguous [Text]
  deriving (Eq, Show)

-- Business logic

give :: Request -> ToolM Response
give = error "un"

-- Request parsing

instance FromJSON Request where
  parseJSON = error "un"

-- Response rendering

renderResponse :: Response -> Text
renderResponse = error "un"
