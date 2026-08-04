{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Source (
  Source (..),
  SourceUnreadable (..),
  SourceUnwritable (..),
  SpliceViolation (..),
  readSource,
  writeSource,
  spliceEdits,
  reindent,
) where

import Agda.Utils.Hash (hashText)
import Agda.Utils.IO.UTF8 qualified as UTF8
import Control.Exception (
  Exception,
  Handler (..),
  IOException,
  catches,
  displayException,
  try,
 )
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lazy qualified as Text.Lazy
import System.AtomicWrite.Writer.ByteString.Binary (atomicWriteFile)

import AgdaMCP.Interaction (Hash, Position (..), Span (..), spanText)

data Source = Source
  { sourceText :: Text
  , sourceHash :: Hash
  }
  deriving (Eq, Show)

newtype SourceUnreadable = SourceUnreadable {sourceUnreadableMessage :: Text}
  deriving (Eq, Show)

newtype SourceUnwritable = SourceUnwritable {sourceUnwritableMessage :: Text}
  deriving (Eq, Show)

-- Read source text and compute a hash the same way Agda does.
readSource :: FilePath -> IO (Either SourceUnreadable Source)
readSource path =
  (Right <$> readSourceFile)
    `catches` [Handler unreadableIO, Handler unreadableDecoding]
 where
  readSourceFile :: IO Source
  readSourceFile = do
    lazy <- UTF8.readTextFile path
    pure $ Source (Text.Lazy.toStrict lazy) (hashText lazy)

  unreadableIO :: IOException -> IO (Either SourceUnreadable Source)
  unreadableIO = pure . Left . rendered

  unreadableDecoding :: UTF8.ReadException -> IO (Either SourceUnreadable Source)
  unreadableDecoding = pure . Left . rendered

  rendered :: (Exception e) => e -> SourceUnreadable
  rendered = SourceUnreadable . Text.pack . displayException

-- TODO: This normalizes CRLFs to LFs. Doing better than that is a future me
-- problem.
writeSource :: FilePath -> Text -> IO (Either SourceUnwritable ())
writeSource path text =
  first failed
    <$> ( try (atomicWriteFile path (Text.Encoding.encodeUtf8 text)) ::
            IO (Either IOException ())
        )
 where
  failed = SourceUnwritable . Text.pack . displayException

spliceEdits :: Text -> [(Span, Text)] -> Either SpliceViolation Text
spliceEdits source edits = do
  let descending = sortOn (Down . positionOffset . spanStart . fst) edits
  checkDisjoint (map fst descending)
  traverse_ (validate . fst) descending
  pure $ foldl' apply source descending
 where
  checkDisjoint :: [Span] -> Either SpliceViolation ()
  checkDisjoint (later : earlier : rest) = do
    unless (positionOffset (spanEnd earlier) <= positionOffset (spanStart later)) $
      Left (SpansOverlap earlier later)
    checkDisjoint (earlier : rest)
  checkDisjoint _ = Right ()

  validate :: Span -> Either SpliceViolation ()
  validate hole = do
    let start = positionOffset (spanStart hole)
        end = positionOffset (spanEnd hole)
    unless (0 <= start && start <= end && end <= Text.length source) $
      Left (SpanOutOfBounds hole (Text.length source))
    let replaced = spanText source hole
    unless (isHole replaced) $ Left (SpanNotHole hole replaced)

  apply :: Text -> (Span, Text) -> Text
  apply text (hole, replacement) =
    Text.take (positionOffset (spanStart hole)) text
      <> reindent (positionColumn (spanStart hole) - 1) replacement
      <> Text.drop (positionOffset (spanEnd hole)) text

isHole :: Text -> Bool
isHole text =
  text == "?"
    || ( Text.isPrefixOf "{!" text
           && Text.isSuffixOf "!}" text
           && Text.length text >= 4
       )

reindent :: Int -> Text -> Text
reindent column text =
  case Text.splitOn "\n" text of
    [] -> text
    firstLine : rest -> Text.intercalate "\n" $ firstLine : map pad rest
 where
  pad line
    | Text.null line = line
    | otherwise = Text.replicate column " " <> line

{- | Bug: a splice was asked to write somewhere that is not a hole in the text
Agda checked.
-}
data SpliceViolation
  = -- The span reaches outside the source text.
    SpanOutOfBounds Span Int
  | -- The text under the span is not a hole.
    SpanNotHole Span Text
  | -- Two edits in one batch cover overlapping text, so neither should be
    -- applied.
    SpansOverlap Span Span
  deriving (Eq, Show)

instance Exception SpliceViolation
