{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Render (
  renderSpan,
  renderShape,
  renderContextEntry,
  renderWarning,
  renderGoalId,
  renderUnknownGoal,
  renderAgdaError,
  renderFileChanged,
  renderSourceUnreadable,
  renderWriteFailed,
  renderSourceRefusal,
  renderReadOnlyTimeout,
  renderEditingTimeout,
  normalizations,
  renderNormalization,
  blocks,
  section,
  indent,
  hanging,
) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Syntax.Common (InteractionId (..))
import Control.Monad (guard)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction (
  ContextEntry (..),
  Error (..),
  GoalShape (..),
  Position (..),
  Span (..),
  Warning (..),
 )
import AgdaMCP.Tools.Source (SourceRefusal (..))

{- | Either "n:a-b" if the start and end lines are the same, or "n:a-m:b"
otherwise.
-}
renderSpan :: Span -> Text
renderSpan s
  | positionLine start == positionLine end =
      renderPosition start <> "-" <> Text.pack (show $ positionColumn end)
  | otherwise = renderPosition start <> "-" <> renderPosition end
 where
  start = spanStart s
  end = spanEnd s

renderPosition :: Position -> Text
renderPosition (Position _ l c) =
  -- Offsets are not rendered
  Text.pack (show l) <> ":" <> Text.pack (show c)

renderShape :: GoalShape -> Text
renderShape (GoalOfType t) = t
renderShape GoalSort = "Sort"

{- | Follows @prettyResponseContext@ (EmacsTop.hs:324-373), minus its @align 10@
padding. We render cohesion as a prefix, the three-form name rule, one
comma-separated attribute group in Agda's order appended after the type, and
a let value on its own line.
-}
renderContextEntry :: ContextEntry -> Text
renderContextEntry entry =
  case contextEntryLetValue entry of
    Nothing -> typed
    Just value -> typed <> "\n" <> hanging (name <> " = ") value
 where
  typed =
    hanging (nameWithMaybeCohesion <> " : ") (contextEntryType entry)
      <> attributeGroup
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

renderWarning :: Warning -> Text
renderWarning (Warning (_, message)) =
  -- The structural location beside the message is not rendered. The message
  -- embeds its own location, so printing both would print it twice.
  indent message

renderGoalId :: InteractionId -> Text
renderGoalId goalId = "?" <> Text.pack (show $ interactionId goalId)

renderUnknownGoal :: InteractionId -> Text
renderUnknownGoal unknownId =
  "No goal "
    <> renderGoalId unknownId
    <> " in the current load. Check the goal IDs in the most recent result."

renderAgdaError :: Text -> Error -> Text
renderAgdaError headline e =
  blocks $
    [headline, indent $ errorMessage e]
      <> section "Warnings:" (map renderWarning $ errorWarnings e)

renderFileChanged :: Text
renderFileChanged =
  "The file on disk no longer matches the source Agda checked, so no edits \
  \were written. It has been reloaded below; goal IDs from the earlier load \
  \are no longer valid."

renderSourceUnreadable :: Text -> Text
renderSourceUnreadable message =
  blocks
    [ "Could not read the file to check it still matches the source Agda \
      \checked, so no edits were written. It has been reloaded below, and \
      \goal IDs from the earlier load are no longer valid."
    , indent message
    ]

renderWriteFailed :: Text -> Text
renderWriteFailed message =
  blocks
    [ "Could not write the file, so it was not modified. It has been \
      \reloaded below, and goal IDs from the earlier load are no longer \
      \valid."
    , indent message
    ]

renderSourceRefusal :: SourceRefusal -> Text
renderSourceRefusal RefusalChanged =
  "The file on disk no longer matches the source Agda checked. Load the file \
  \again, then use the load ID and goal IDs from that load result."
renderSourceRefusal (RefusalUnreadable message) =
  blocks
    [ "Could not read the file to check it still matches the source Agda \
      \checked. Load the file again once it is readable, then use the load ID \
      \and goal IDs from that load result."
    , indent message
    ]

renderReadOnlyTimeout :: Int -> Text
renderReadOnlyTimeout seconds =
  "The call ran longer than its timeout of "
    <> renderSeconds seconds
    <> " and was canceled. Nothing was modified, and the file loaded before \
       \the call is still the current one, so its load ID and goal IDs remain \
       \valid. Retry with a larger `timeout` if the work needs longer."

renderEditingTimeout :: Int -> Text
renderEditingTimeout seconds =
  "The call ran longer than its timeout of "
    <> renderSeconds seconds
    <> " and was canceled. If it had already written its edits, the file on \
       \disk has changed but has not been checked, and the other tools refuse \
       \to work against the earlier load until you call `load` again. Load the \
       \file to see where it stands, and retry with a larger `timeout` if the \
       \work needs longer."

-- | The normalization levels by the names both front ends accept.
normalizations :: Map Text Rewrite
normalizations =
  Map.fromList
    [ ("asis", AsIs)
    , ("instantiated", Instantiated)
    , ("headnormal", HeadNormal)
    , ("simplified", Simplified)
    , ("normalized", Normalised)
    ]

renderNormalization :: Rewrite -> Text
renderNormalization AsIs = "asis"
renderNormalization Instantiated = "instantiated"
renderNormalization HeadNormal = "headnormal"
renderNormalization Simplified = "simplified"
renderNormalization Normalised = "normalized"

renderSeconds :: Int -> Text
renderSeconds 1 = "1 second"
renderSeconds seconds = Text.pack (show seconds) <> " seconds"

-- | Blocks are separated by one blank line.
blocks :: [Text] -> Text
blocks = Text.intercalate "\n\n"

-- | A titled section around its already-indented items, or the empty string.
section :: Text -> [Text] -> [Text]
section _ [] = []
section title items = [title <> "\n" <> blocks items]

-- | Every line, so multi-line payloads keep their own internal indentation.
indent :: Text -> Text
indent = Text.intercalate "\n" . map ("  " <>) . Text.splitOn "\n"

{- | The prefix followed by the body, with each of the body's later lines
padded to start under the column where its first line started. The padding is
one space per code point of the prefix, and is added to whatever leading
spaces the line already carries.
-}
hanging :: Text -> Text -> Text
hanging prefix body =
  prefix
    <> Text.replace "\n" ("\n" <> Text.replicate (Text.length prefix) " ") body
