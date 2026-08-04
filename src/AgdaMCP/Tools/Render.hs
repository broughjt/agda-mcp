{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Render (
  renderSpan,
  renderShape,
  renderContextEntry,
  renderWarning,
  renderFileChanged,
  renderSourceUnreadable,
  renderWriteFailed,
  blocks,
  section,
  indent,
) where

import Control.Monad (guard)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction (
  ContextEntry (..),
  GoalShape (..),
  Position (..),
  Span (..),
  Warning (..),
 )

-- Either n:a-b if the start and end lines are the same, or n:a-m:b otherwise.
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

-- The structural location beside the message is not rendered: the message
-- embeds its own location, so printing both would print it twice.
renderWarning :: Warning -> Text
renderWarning (Warning (_, message)) = indent message

renderFileChanged :: Text
renderFileChanged =
  "The file on disk no longer matches the source Agda checked, so no edits \
  \were written. It has been reloaded below; goal IDs from the earlier load \
  \are no longer valid."

renderSourceUnreadable :: Text -> Text
renderSourceUnreadable message =
  blocks
    [ "Could not read the file to check it still matches the source Agda \
      \checked, so no edits were written."
    , indent message
    ]

renderWriteFailed :: Text -> Text
renderWriteFailed message =
  blocks
    [ "Could not write the file. It was not modified."
    , indent message
    ]

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
