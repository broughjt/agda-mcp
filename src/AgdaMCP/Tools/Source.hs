{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Source (
  Source (..),
  SourceRefusal (..),
  SourceUnreadable (..),
  SourceUnwritable (..),
  SpliceViolation (..),
  readSource,
  readChecked,
  writeSource,
  commitEdits,
  spliceEdits,
  checkHole,
  checkClauseExtent,
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
  throwIO,
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

data SourceRefusal
  = RefusalUnreadable Text
  | RefusalChanged
  deriving (Eq, Show)

readChecked :: FilePath -> Hash -> IO (Either SourceRefusal Text)
readChecked path expected = do
  found <- readSource path
  pure $ case found of
    Left unreadable ->
      Left $ RefusalUnreadable $ sourceUnreadableMessage unreadable
    Right file
      | sourceHash file == expected -> Right $ sourceText file
      | otherwise -> Left RefusalChanged

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

commitEdits ::
  FilePath -> Text -> [(Span, Text)] -> IO (Either SourceUnwritable ())
commitEdits path original edits =
  either throwIO (writeSource path) (spliceEdits original edits)

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
  validate span' = do
    let start = positionOffset (spanStart span')
        end = positionOffset (spanEnd span')
    unless (0 <= start && start <= end && end <= Text.length source) $
      Left (SpanOutOfBounds span' (Text.length source))

  apply :: Text -> (Span, Text) -> Text
  apply text (hole, replacement) =
    Text.take (positionOffset (spanStart hole)) text
      <> reindent (positionColumn (spanStart hole) - 1) replacement
      <> Text.drop (positionOffset (spanEnd hole)) text

checkHole :: Text -> Span -> Either SpliceViolation ()
checkHole source hole =
  unless (isHole toBeReplaced) $ Left (SpanNotHole hole toBeReplaced)
 where
  toBeReplaced = spanText source hole

checkClauseExtent :: Text -> Span -> Either SpliceViolation ()
checkClauseExtent source extent =
  unless (containsHole replaced) $ Left (SpanNotClause extent replaced)
 where
  replaced = spanText source extent
  containsHole text = "?" `Text.isInfixOf` text || "{!" `Text.isInfixOf` text

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

data SpliceViolation
  = SpanOutOfBounds Span Int
  | SpanNotHole Span Text
  | SpanNotClause Span Text
  | SpansOverlap Span Span
  deriving (Eq, Show)

instance Exception SpliceViolation
