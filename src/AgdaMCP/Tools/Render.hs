{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Render (
  renderSpan,
) where

import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction (Position (..), Span (..))

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
