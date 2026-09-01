{-# LANGUAGE OverloadedStrings #-}

-- | Help text rendered from tool descriptions.
module AgdaMCP.Interact.Help (
  renderShape,
  renderVariant,
  outputSection,
  shapesSection,
  unknownTool,
  status,
  stopping,
) where

import Data.Text (Text)

import AgdaMCP.Tools.Description (Description, Shape, Variant)

{- | A shape in one line: @T@, @N@, @B@, an enum as its alternatives, an
array in brackets, an object as its fields, a nullable with @| null@, and a
named leaf or nested sum by its name.
-}
renderShape :: Shape -> Text
renderShape = error "not implemented"

-- | A variant as its object, @kind@ first.
renderVariant :: Variant -> Text
renderVariant = error "not implemented"

{- | A verb's @Output@ section: its answers, its refusals if any, then each
nested sum once, in order of first mention.
-}
outputSection :: Description -> Text
outputSection = error "not implemented"

-- | The named leaves as a two-column table, for the top-level help.
shapesSection :: [(Text, Shape)] -> Text
shapesSection = error "not implemented"

-- | The answers the daemon gives that are not a tool's.
unknownTool, status, stopping :: Variant
unknownTool = error "not implemented"
status = error "not implemented"
stopping = error "not implemented"
