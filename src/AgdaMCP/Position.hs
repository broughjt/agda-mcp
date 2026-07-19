module AgdaMCP.Position (
  Position (..),
  Span (..),
  toPosition,
  toSpan,
  rangePath,
  rangeSpan,
  rangePathSpan,
) where

import Agda.Syntax.Position (
  Range,
  RangeFile (..),
  rangeFile,
  rangeToInterval,
 )
import Agda.Syntax.Position qualified
import Agda.Utils.FileName (filePath)
import Agda.Utils.Maybe.Strict qualified as Strict

-- A position in a loaded file, consisting of a zero-based offset into the
-- Agda-normalized source text (what `applyEdits` splices; see the note in
-- `commit`) and the one-based line/column that Agda prints. Agda's `posPos` is
-- one-based, hence the subtraction in `toPosition`.
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

-- fileSpan :: AbsolutePath -> Agda.Syntax.Position.Range -> Maybe Span
-- fileSpan p r = do
--   rangeFile <- Strict.toLazy $ Agda.Syntax.Position.rangeFile r
--   guard $ Agda.Syntax.Position.rangeFilePath rangeFile == p
--   toSpan <$> Agda.Syntax.Position.rangeToInterval r

-- rangeFile' :: Range' p -> Maybe p
-- rangeFile' NoRange = Nothing
-- rangeFile' (Range p _) = Just p

rangePath :: Range -> Maybe FilePath
rangePath = fmap (filePath . rangeFilePath) . Strict.toLazy . rangeFile

rangeSpan :: Range -> Maybe Span
rangeSpan = fmap toSpan . rangeToInterval

rangePathSpan :: Agda.Syntax.Position.Range -> Maybe (FilePath, Span)
rangePathSpan r = (,) <$> rangePath r <*> rangeSpan r

-- rangeMaybeToFileSpan Agda.Syntax.Position.NoRange = error "unimplemented"
-- rangeMaybeToFileSpan r@(Agda.Syntax.Position.Range a b) =
--   let foo = rangeToInterval r
--    in error "unimplemented"

-- spanText :: Text -> Span -> Text
-- spanText t s =
--   Text.take
--     (spanLength s)
--     (Text.drop (positionOffset (spanStart s)) t)

-- spanLength :: Span -> Int
-- spanLength s = positionOffset (spanEnd s) - positionOffset (spanStart s)

-- renderSpan :: Span -> Text
-- renderSpan s
--   | positionLine start == positionLine end =
--       renderPosition start <> "-" <> Text.pack (show (positionColumn end))
--   | otherwise = renderPosition start <> "-" <> renderPosition end
--  where
--   start = spanStart s
--   end = spanEnd s

-- renderPosition :: Position -> Text
-- renderPosition (Position _ l c) =
--   Text.pack (show l) <> ":" <> Text.pack (show c)
