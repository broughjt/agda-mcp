{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Position (
  Position (..),
  Span (..),
  fileSpan,
  renderPosition,
  renderSpan,
  spanLength,
  spanText,
  toSpan,
) where

import Control.Monad (guard)
import Data.Text (Text)
import Data.Text qualified as Text

import Agda.Syntax.Position qualified
import Agda.Utils.FileName (AbsolutePath)
import Agda.Utils.Maybe.Strict qualified as Strict

-- A position in the loaded file, as both a 0-based code-point offset into the
-- Agda-normalized source text (what `applyEdits` splices with; see the note
-- in `commit` about normalization) and the 1-based line/column that Agda
-- prints. Agda's `posPos` is 1-based, hence the shift in `toPos`.

-- A position in a loaded file, consisting of a zero-based offset into the
-- Agda-normalized source text (see comment in `commit`) and one-based
-- line/column that Agda prints.
--
-- We do this because Agda's one-based positions are weird.
data Position = Position
  { positionOffset :: Int
  , positionLine :: Int
  , positionColumn :: Int
  }
  deriving (Eq, Show)

-- A contiguous part of the loaded file with start inclusive and end exclusive.
data Span = Span
  { spanStart :: Position
  , spanEnd :: Position
  }
  deriving (Eq, Show)

toPosition :: Agda.Syntax.Position.PositionWithoutFile -> Position
toPosition p =
  Position
    { positionOffset = fromIntegral (Agda.Syntax.Position.posPos p) - 1
    , positionLine = fromIntegral (Agda.Syntax.Position.posLine p)
    , positionColumn = fromIntegral (Agda.Syntax.Position.posCol p)
    }

toSpan :: Agda.Syntax.Position.IntervalWithoutFile -> Span
toSpan i =
  Span
    (toPosition (Agda.Syntax.Position.iStart i))
    (toPosition (Agda.Syntax.Position.iEnd i))

fileSpan :: AbsolutePath -> Agda.Syntax.Position.Range -> Maybe Span
fileSpan p r = do
  rangeFile <- Strict.toLazy $ Agda.Syntax.Position.rangeFile r
  guard $ Agda.Syntax.Position.rangeFilePath rangeFile == p
  toSpan <$> Agda.Syntax.Position.rangeToInterval r

spanText :: Text -> Span -> Text
spanText t s =
  Text.take
    (spanLength s)
    (Text.drop (positionOffset (spanStart s)) t)

spanLength :: Span -> Int
spanLength s = positionOffset (spanEnd s) - positionOffset (spanStart s)

renderSpan :: Span -> Text
renderSpan s
  | positionLine start == positionLine end =
      renderPosition start <> "-" <> Text.pack (show (positionColumn end))
  | otherwise = renderPosition start <> "-" <> renderPosition end
 where
  start = spanStart s
  end = spanEnd s

renderPosition :: Position -> Text
renderPosition (Position _ l c) =
  Text.pack (show l) <> ":" <> Text.pack (show c)
