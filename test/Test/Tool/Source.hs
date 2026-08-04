{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Source (tests) where

import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import AgdaMCP.Interaction (Position (..), Span (..))
import AgdaMCP.Tools.Source (
  Source (..),
  SpliceViolation (..),
  readSource,
  reindent,
  spliceEdits,
  writeSource,
 )

tests :: TestTree
tests =
  testGroup
    "Source"
    [ testGroup
        "spliceEdits"
        [ testCase "a hole is replaced by its text" $
            spliceEdits
              (source ["f : ℕ", "f = ?", ""])
              [(Span (Position 10 2 5) (Position 11 2 6), "refl")]
              @?= Right (source ["f : ℕ", "f = refl", ""])
        , testCase "offsets count code points rather than bytes" $
            spliceEdits
              (source ["f : ℕ → ℕ → ℕ", "f = λ x y → ?", ""])
              [(Span (Position 26 2 13) (Position 27 2 14), "refl")]
              @?= Right (source ["f : ℕ → ℕ → ℕ", "f = λ x y → refl", ""])
        , testCase "a braced hole is replaced along with its delimiters" $
            spliceEdits
              (source ["f = {! !}", ""])
              [(Span (Position 4 1 5) (Position 9 1 10), "refl")]
              @?= Right (source ["f = refl", ""])
        , testCase "wrapped text is indented to the hole's column" $
            spliceEdits
              (source ["f : ℕ", "f = ?", ""])
              [(Span (Position 10 2 5) (Position 11 2 6), "trans a\n(sym b)")]
              @?= Right
                ( source
                    [ "f : ℕ"
                    , "f = trans a"
                    , "    (sym b)"
                    , ""
                    ]
                )
        , testCase "two holes on one line are both replaced" $
            spliceEdits
              (source ["g = ? + ?", ""])
              [ (Span (Position 4 1 5) (Position 5 1 6), "zero")
              , (Span (Position 8 1 9) (Position 9 1 10), "suc zero")
              ]
              @?= Right (source ["g = zero + suc zero", ""])
        , testCase "the order the edits arrive in does not matter" $
            spliceEdits
              (source ["g = ? + ?", ""])
              [ (Span (Position 8 1 9) (Position 9 1 10), "suc zero")
              , (Span (Position 4 1 5) (Position 5 1 6), "zero")
              ]
              @?= spliceEdits
                (source ["g = ? + ?", ""])
                [ (Span (Position 4 1 5) (Position 5 1 6), "zero")
                , (Span (Position 8 1 9) (Position 9 1 10), "suc zero")
                ]
        , testCase "two holes on separate lines are both replaced" $
            spliceEdits
              (source ["a = ?", "b = ?", ""])
              [ (Span (Position 4 1 5) (Position 5 1 6), "zero")
              , (Span (Position 10 2 5) (Position 11 2 6), "refl")
              ]
              @?= Right (source ["a = zero", "b = refl", ""])
        , testCase "a later hole on the same line keeps its own indent column" $
            spliceEdits
              (source ["g = ? + ?", ""])
              [ (Span (Position 4 1 5) (Position 5 1 6), "x\ny")
              , (Span (Position 8 1 9) (Position 9 1 10), "p\nq")
              ]
              @?= Right
                ( source
                    [ "g = x"
                    , "    y + p"
                    , "        q"
                    , ""
                    ]
                )
        , testCase "a span over something that is not a hole is refused" $
            spliceEdits
              (source ["f : ℕ", "f = ?", ""])
              [(Span (Position 0 1 1) (Position 2 1 3), "refl")]
              @?= Left
                (SpanNotHole (Span (Position 0 1 1) (Position 2 1 3)) "f ")
        , testCase "a span reaching past the end of the text is refused" $
            spliceEdits
              (source ["f : ℕ", "f = ?", ""])
              [(Span (Position 10 2 5) (Position 99 2 6), "refl")]
              @?= Left
                (SpanOutOfBounds (Span (Position 10 2 5) (Position 99 2 6)) 12)
        , testCase "overlapping spans are refused" $
            spliceEdits
              (source ["g = ? + ?", ""])
              [ (Span (Position 4 1 5) (Position 6 1 7), "x")
              , (Span (Position 5 1 6) (Position 7 1 8), "y")
              ]
              @?= Left
                ( SpansOverlap
                    (Span (Position 4 1 5) (Position 6 1 7))
                    (Span (Position 5 1 6) (Position 7 1 8))
                )
        , testCase "no edits leaves the source alone" $
            spliceEdits (source ["f : ℕ", "f = ?", ""]) []
              @?= Right (source ["f : ℕ", "f = ?", ""])
        ]
    , testGroup
        "reindent"
        [ testCase "single-line text is untouched" $
            reindent 4 "refl" @?= "refl"
        , testCase "every line after the first is indented" $
            reindent 4 "a\nb\nc" @?= "a\n    b\n    c"
        , testCase "a hole in the first column indents nothing" $
            reindent 0 "a\nb" @?= "a\nb"
        , testCase "blank lines are left blank rather than padded" $
            reindent 4 "a\n\nb" @?= "a\n\n    b"
        , testCase "the text's own internal indentation is preserved" $
            reindent 2 "a\n  b" @?= "a\n    b"
        ]
    , testGroup
        "readSource and writeSource"
        [ testCase "a write round-trips through a read, non-ASCII included" $
            withSystemTempDirectory "agda-mcp-source" $ \directory -> do
              let path = directory </> "Round.agda"
                  text = source ["f : ℕ → ℕ", "f = λ x → x ≡ x", ""]
              writeSource path text >>= expectWritten
              written <- readSource path >>= expectRead
              sourceText written @?= text
        , testCase "line endings are normalized, and the hash is over the normalization" $
            withSystemTempDirectory "agda-mcp-source" $ \directory -> do
              let crlfPath = directory </> "Crlf.agda"
                  lfPath = directory </> "Lf.agda"
              writeFile crlfPath "f : Nat\r\nf = ?\r\n"
              writeFile lfPath "f : Nat\nf = ?\n"
              crlf <- readSource crlfPath >>= expectRead
              lf <- readSource lfPath >>= expectRead
              sourceText crlf @?= source ["f : Nat", "f = ?", ""]
              sourceHash crlf @?= sourceHash lf
        , testCase "a missing file is an outcome rather than an exception" $
            withSystemTempDirectory "agda-mcp-source" $ \directory -> do
              found <- readSource (directory </> "Absent.agda")
              case found of
                Left _ -> pure ()
                Right read' ->
                  assertFailure $ "expected a refusal, got " <> show read'
        ]
    ]
 where
  expectWritten = either (assertFailure . ("write failed: " <>) . show) pure
  expectRead = either (assertFailure . ("read failed: " <>) . show) pure

source :: [Text] -> Text
source = Text.intercalate "\n"
