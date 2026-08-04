{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.CaseSplit (
  caseSplitTool,
  Request (..),
  Response (..),
  Outcome (..),
  Edit (..),
  ClauseLayout (..),
  caseSplit,
  clauseLayout,
  layoutClauses,
  renderResponse,
) where

import Agda.Syntax.Common (InteractionId (..))
import Data.Aeson (FromJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )

import AgdaMCP.Interaction (Error, Span)
import AgdaMCP.Interaction.MakeCase (MakeCaseVariant)
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadId, LoadIdRefusal)
import AgdaMCP.Tools.MCP (
  goalIdSchema,
  loadIdSchema,
  textToolHandle,
 )
import AgdaMCP.Tools.State (ToolM)

caseSplitTool :: ToolHandler
caseSplitTool =
  toolHandler
    "case_split"
    ( Just
        "Case split an open goal in the currently loaded Agda file, replacing \
        \the clause the goal sits in with one clause per constructor. Pass the \
        \pattern variables to split on; an empty list instead introduces the \
        \clause's remaining arguments, or splits on the result if they are \
        \already introduced. A variable that is not currently in scope is \
        \revealed as a visible pattern rather than split on. The replacement is \
        \written to the file and the file is reloaded, so goal IDs are \
        \renumbered: use the goals in the result. Goal IDs are only meaningful \
        \against the load that issued them, so pass the `load_id` from that \
        \load result; an ID from an earlier load is refused. Unlike `give`, a \
        \split can produce a file that no longer typechecks — if the clause \
        \carried a `where` block, the block ends up attached to the last \
        \generated clause only, and the result says so. In that case the file \
        \has still been modified, and the reload below will report the error."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [ ("load_id", loadIdSchema)
              , ("goal", goalIdSchema)
              ,
                ( "variables"
                , object
                    [ "type" .= ("array" :: Text)
                    , "items" .= object ["type" .= ("string" :: Text)]
                    , "description"
                        .= ( "The pattern variables to split on, by name. An \
                             \empty list introduces the clause's remaining \
                             \arguments instead, or splits on the result if \
                             \they are already introduced." ::
                               Text
                           )
                    ]
                )
              ]
        )
        (Just ["load_id", "goal", "variables"])
    )
    (textToolHandle caseSplit renderResponse)

data Request = Request
  { caseSplitRequestLoadId :: LoadId
  , caseSplitRequestGoalId :: InteractionId
  , caseSplitRequestVariables :: [Text]
  {- ^ Empty introduces the clause's remaining arguments, or splits on the
  result if they are already introduced.
  -}
  }
  deriving (Eq, Show)

data Response
  = ResponseRefused LoadIdRefusal
  | ResponseCompleted InteractionId Outcome Load.Response
  deriving (Eq, Show)

data Outcome
  = OutcomeApplied Edit
  | -- No interaction point with that id in the current load.
    OutcomeUnknownGoal
  | {- The split itself failed due to a mistake of the caller. For example, the
    goal was not in a clause, a named variable was unbound or not splittable, or
    the clause held a give since the last load. -}
    OutcomeFailed Error
  | OutcomeFileChanged
  | OutcomeSourceUnreadable Text
  | OutcomeWriteFailed Text
  deriving (Eq, Show)

data Edit = Edit
  { editSpan :: Span
  {- ^ The clause extent that was replaced, from the start of the left-hand
  side to the end of the right-hand side. A `where` block is outside it, since
  Agda does not regenerate one.
  -}
  , editVariant :: MakeCaseVariant
  , editLayout :: ClauseLayout
  , editClauses :: [Text]
  -- ^ As produced by Agda before `layoutClauses` joined them.
  , editCollapsesWhere :: Bool
  {- ^ The split clause carried a `where` block, which now belongs to the last
  generated clause alone.
  -}
  }
  deriving (Eq, Show)

-- How the replacement clauses were joined. See `clauseLayout`.
data ClauseLayout = OnePerLine | Inline
  deriving (Eq, Show)

-- Business logic

caseSplit :: Request -> ToolM Response
caseSplit = error "un"

{- | Which layout the clauses replacing this extent must use.

Emacs decides by scanning backwards from the hole for a `;` or an opening brace
(`agda2-make-case-action-extendlam`, agda2-mode.el:930-953). We have the extent,
so the same question is just whether it starts at its line's first
non-whitespace column. That covers ordinary function clauses, `λ where` and
`λ { }` alike, so `MakeCaseVariant` does not drive layout.
-}
clauseLayout :: Text -> Span -> ClauseLayout
clauseLayout = error "un"

layoutClauses :: ClauseLayout -> [Text] -> Text
layoutClauses = error "un"

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "case_split arguments" $ \o ->
    Request
      <$> o .: "load_id"
      <*> (InteractionId <$> o .: "goal")
      <*> Aeson.explicitParseField parseVariables o "variables"

parseVariables :: Aeson.Value -> Aeson.Parser [Text]
parseVariables = Aeson.withArray "variables" $ \values ->
  traverse
    (\(index, value) -> parseVariable value Aeson.<?> Aeson.Index index)
    (zip [0 ..] $ toList values)

parseVariable :: Aeson.Value -> Aeson.Parser Text
parseVariable = Aeson.withText "variable" validateVariable

{- | Reject names that would select a different split than the one asked for.
The wrapper re-encodes the list as the string Agda splits with `words`, so `"."`
would reach the ellipsis branch this tool does not expose, and a name carrying
whitespace would silently become a different number of variables.
-}
validateVariable :: Text -> Aeson.Parser Text
validateVariable name
  | name == "." =
      fail
        "expected a pattern variable name, but got \".\", which Agda reads as \
        \the ellipsis of a with-clause rather than as a variable"
  | Text.words name == [name] = pure name
  | null (Text.words name) =
      fail $
        "expected a pattern variable name, but got " <> show name
  | length (Text.words name) == 1 =
      fail $
        "expected a pattern variable name with no surrounding whitespace, but \
        \got "
          <> show name
  | otherwise =
      fail $
        "expected a single pattern variable name, but got "
          <> show name
          <> ", which is "
          <> show (length $ Text.words name)
          <> " names; send each as its own entry"

-- Response rendering

renderResponse :: Response -> Either Text Text
renderResponse = error "un"
