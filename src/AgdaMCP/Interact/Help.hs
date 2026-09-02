{-# LANGUAGE OverloadedStrings #-}

-- | Help text rendered from tool descriptions.
module AgdaMCP.Interact.Help (
  renderShape,
  renderVariant,
  variantEntries,
  outputSection,
  shapesSection,
  unknownTool,
  status,
  stopping,
) where

import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Tools.Description (
  Description (..),
  Field (..),
  Shape (..),
  Variant (..),
  field,
 )

{- | A shape in one line: @T@, @N@, @B@, an enum as its alternatives, an
array in brackets, an object as its fields, a nullable with @| null@, and a
named leaf or nested sum by its name.
-}
renderShape :: Shape -> Text
renderShape ShapeString = "T"
renderShape ShapeInteger = "N"
renderShape ShapeBoolean = "B"
renderShape (ShapeEnum alternatives) =
  Text.intercalate " | " $ map quoted alternatives
renderShape (ShapeArray element) = "[" <> renderShape element <> "]"
renderShape (ShapeObject fields) = renderObject $ map renderField fields
renderShape (ShapeNullable shape) = renderShape shape <> " | null"
renderShape (ShapeNamed name) = name
renderShape (ShapeSum name _) = name

-- | A variant as its object, @kind@ first.
renderVariant :: Variant -> Text
renderVariant variant =
  renderObject $
    (quoted "kind" <> ": " <> quoted (variantKind variant))
      : map renderField (variantFields variant)

quoted :: Text -> Text
quoted text = "\"" <> text <> "\""

renderField :: Field -> Text
renderField f = quoted (fieldName f) <> ": " <> renderShape (fieldShape f)

renderObject :: [Text] -> Text
renderObject entries = "{" <> Text.intercalate ", " entries <> "}"

{- | Each variant on its own line, indented as a help section's entries, with
its summary indented further beneath it.
-}
variantEntries :: [Variant] -> [Text]
variantEntries = concatMap entry
 where
  entry variant =
    ["    " <> renderVariant variant, "        " <> variantSummary variant]

{- | A verb's @Output@ section: its answers, its refusals if any, then each
nested sum once, in order of first mention.
-}
outputSection :: Description -> Text
outputSection description =
  Text.unlines $
    "Output:"
      : block "Answers, exit 0:" (descriptionAnswers description)
        <> (if null refusals then [] else block "Refusals, exit 1:" refusals)
        <> concatMap
          (\(name, variants) -> block (name <> " is one of:") variants)
          (nestedSums description)
 where
  refusals = descriptionRefusals description
  block title variants = ("  " <> title) : variantEntries variants

{- | The nested sums a description mentions, each once, in the order a reader
meets them: those named by the answers and refusals first, then those named
inside each of them, as the help text expands them.
-}
nestedSums :: Description -> [(Text, [Variant])]
nestedSums description =
  expand [] $
    sumsIn $
      descriptionAnswers description <> descriptionRefusals description
 where
  expand _ [] = []
  expand seen ((name, variants) : queued)
    | name `elem` seen = expand seen queued
    | otherwise =
        (name, variants) : expand (name : seen) (queued <> sumsIn variants)

  sumsIn :: [Variant] -> [(Text, [Variant])]
  sumsIn = concatMap $ concatMap (inShape . fieldShape) . variantFields

  inShape (ShapeSum name variants) = [(name, variants)]
  inShape (ShapeArray element) = inShape element
  inShape (ShapeNullable shape) = inShape shape
  inShape (ShapeObject fields) = concatMap (inShape . fieldShape) fields
  inShape _ = []

-- | The named leaves as a two-column table, for the top-level help.
shapesSection :: [(Text, Shape)] -> Text
shapesSection shapes =
  Text.unlines $ "Shapes named in verb output:" : map row shapes
 where
  width = maximum $ 0 : map (Text.length . fst) shapes
  row (name, shape) =
    "  " <> Text.justifyLeft width ' ' name <> "  " <> definition shape
  -- A named sum is defined by its alternatives, since its name is the entry.
  definition (ShapeSum _ variants) =
    Text.intercalate " | " $ map renderVariant variants
  definition shape = renderShape shape

-- | The answers the daemon gives that are not a tool's.
unknownTool, status, stopping :: Variant
unknownTool =
  Variant
    "unknown_tool"
    "no tool has that name"
    [field "tool" ShapeString, field "message" ShapeString]
status =
  Variant
    "status"
    "the daemon's current load, or null when nothing is loaded"
    [ field
        "load"
        ( ShapeNullable $
            ShapeObject [field "load_id" ShapeString, field "path" ShapeString]
        )
    ]
stopping = Variant "stopping" "the daemon exits once it has answered" []
