{-# LANGUAGE OverloadedStrings #-}

-- | Tool descriptions that the MCP server tool definitions and CLI help share.
module AgdaMCP.Tools.Description (
  Description (..),
  Field (..),
  field,
  parameter,
  Shape (..),
  Variant (..),
  Effect (..),
  parameterDescription,
  overview,
  loadIdField,
  goalIdField,
  normalizationField,
  normalizationShape,
  timeoutField,
  noCurrentLoad,
  staleLoadId,
  foreignLoadId,
  sourceChanged,
  sourceUnreadable,
  unknownGoal,
  loadIdRefusals,
  sourceRefusals,
  invalidArguments,
  timedOut,
  namedShapes,
) where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Tools.Render (normalizations)

data Description = Description
  { descriptionName :: Text
  , descriptionSummary :: Text
  , descriptionParameters :: [Field]
  -- ^ The request's top-level fields.
  , descriptionAnswers :: [Variant]
  -- ^ The values answered with @ok: true@.
  , descriptionRefusals :: [Variant]
  -- ^ The values answered with @ok: false@, each carrying @message@.
  , descriptionEffect :: Effect
  }
  deriving (Eq, Show)

data Field = Field
  { fieldName :: Text
  , fieldShape :: Shape
  , fieldDescription :: Maybe Text
  -- ^ Present on every parameter.
  , fieldRequired :: Bool
  }
  deriving (Eq, Show)

-- | A required, undescribed field, as every output field is.
field :: Text -> Shape -> Field
field name shape =
  Field
    { fieldName = name
    , fieldShape = shape
    , fieldDescription = Nothing
    , fieldRequired = True
    }

-- | A required, described field.
parameter :: Text -> Text -> Shape -> Field
parameter name description shape =
  (field name shape) {fieldDescription = Just description}

data Shape
  = ShapeString
  | ShapeEnum [Text]
  | ShapeInteger
  | ShapeBoolean
  | ShapeArray Shape
  | ShapeObject [Field]
  | ShapeNullable Shape
  | -- | One of `namedShapes`, by name.
    ShapeNamed Text
  | -- | A nested sum tagged by @kind@, under the name help refers to it by.
    ShapeSum Text [Variant]
  deriving (Eq, Show)

-- | One constructor of a @kind@-tagged sum: the fields beside @kind@.
data Variant = Variant
  { variantKind :: Text
  , variantSummary :: Text
  , variantFields :: [Field]
  }
  deriving (Eq, Show)

-- | What a call does to the session, from which the MCP hints follow.
data Effect = Inspects | Loads | Edits
  deriving (Eq, Show)

-- | The description of the named parameter. Fails on a name not described.
parameterDescription :: Text -> Description -> Text
parameterDescription name description =
  case [d | Field n _ (Just d) _ <- descriptionParameters description, n == name] of
    d : _ -> d
    [] ->
      error $
        "no parameter "
          <> show name
          <> " in the description of "
          <> Text.unpack (descriptionName description)

-- | How the tools fit together, for the MCP server's instructions and the CLI's help.
overview :: Text
overview =
  "Tools for developing Agda proofs interactively. Use `load` to type-check a file and see its open goals, inspect them with `goal`, try expressions against them with `check` and `infer`, and fill them with `give` and `case_split`. Agda keeps only one file loaded at a time, and each load reports a load ID that the other tools require. `goal`, `check`, and `infer` do not modify anything, while `give` and `case_split` modify the file on disk and reload it, which issues a new load ID and fresh goal ID assignments."

-- Fields shared by several tools

loadIdField :: Field
loadIdField =
  parameter
    "load_id"
    "The load ID from the load result that issued this goal ID. Each load makes fresh goal ID assignments, so use the most recently issued load ID and goal IDs. If you no longer have that result, load the file again."
    ShapeString

goalIdField :: Field
goalIdField =
  parameter
    "goal"
    "The target goal's interaction ID (the N in `?N`)."
    ShapeInteger

normalizationField :: Field
normalizationField =
  ( parameter
      "normalization"
      "How much to normalize reported types. Defaults to `simplified`, which is what Agda's interactive goal display shows, but which does not unfold definitions. `normalized` unfolds definitions all the way, while `asis` reports types exactly as written."
      normalizationShape
  )
    { fieldRequired = False
    }

-- | The normalization levels, as the parameter takes them and answers name them.
normalizationShape :: Shape
normalizationShape = ShapeEnum $ Map.keys normalizations

-- | Takes the tool's default in seconds.
timeoutField :: Int -> Field
timeoutField seconds =
  ( parameter
      "timeout"
      ( "How long this call may run, in seconds, before the server cancels it. Defaults to "
          <> Text.pack (show seconds)
          <> "."
      )
      ShapeInteger
  )
    { fieldRequired = False
    }

-- Refusals shared by several tools

messageField :: Field
messageField = field "message" ShapeString

noCurrentLoad, staleLoadId, foreignLoadId :: Variant
noCurrentLoad =
  Variant "no_current_load" "no load is current" [messageField]
staleLoadId =
  Variant
    "stale_load_id"
    "the load ID is from an earlier load; the current one is named"
    [field "current_load_id" ShapeString, messageField]
foreignLoadId =
  Variant
    "foreign_load_id"
    "the load ID is from a session that no longer exists"
    [messageField]

sourceChanged, sourceUnreadable, unknownGoal :: Variant
sourceChanged =
  Variant
    "source_changed"
    "the file on disk no longer matches the source Agda checked"
    [messageField]
sourceUnreadable =
  Variant
    "source_unreadable"
    "the file could not be read to check it still matches"
    [field "detail" ShapeString, messageField]
unknownGoal =
  Variant
    "unknown_goal"
    "no such goal in the current load"
    [field "goal" ShapeInteger, messageField]

-- | The refusals of every tool that takes a load ID, in the order they are checked.
loadIdRefusals :: [Variant]
loadIdRefusals = [noCurrentLoad, staleLoadId, foreignLoadId]

-- | The refusals of every tool that reads the loaded file before answering.
sourceRefusals :: [Variant]
sourceRefusals = [sourceChanged, sourceUnreadable]

-- | The answers the adapter gives before a tool runs.
invalidArguments, timedOut :: Variant
invalidArguments =
  Variant "invalid_arguments" "the arguments did not decode" [messageField]
timedOut =
  Variant
    "timed_out"
    "the call ran longer than its timeout and was canceled"
    [field "seconds" ShapeInteger, messageField]

-- | The leaves the tool values share, by the names their descriptions use.
namedShapes :: [(Text, Shape)]
namedShapes =
  [
    ( "POSITION"
    , ShapeObject
        [ field "offset" ShapeInteger
        , field "line" ShapeInteger
        , field "column" ShapeInteger
        ]
    )
  ,
    ( "SPAN"
    , ShapeObject
        [field "start" (ShapeNamed "POSITION"), field "end" (ShapeNamed "POSITION")]
    )
  ,
    ( "SHAPE"
    , ShapeSum
        "SHAPE"
        [ Variant "of_type" "a goal of the given type" [field "type" ShapeString]
        , Variant "sort" "a goal that is a sort" []
        ]
    )
  ,
    ( "ENTRY"
    , ShapeObject
        [ field "original_name" ShapeString
        , field "reified_name" ShapeString
        , field "original_in_scope" ShapeBoolean
        , field "reified_in_scope" ShapeBoolean
        , field "type" ShapeString
        , field "let_value" (ShapeNullable ShapeString)
        , field "is_instance" ShapeBoolean
        , field "cohesion" (ShapeNullable ShapeString)
        , field "polarity" (ShapeNullable ShapeString)
        , field "erased" ShapeBoolean
        , field "relevance" (ShapeNullable ShapeString)
        ]
    )
  ,
    ( "ERROR"
    , ShapeObject
        [ field "message" ShapeString
        , field "location" location
        , field "warnings" (ShapeArray (ShapeNamed "WARNING"))
        ]
    )
  ,
    ( "WARNING"
    , ShapeObject [field "message" ShapeString, field "location" location]
    )
  ,
    ( "HIDDEN"
    , ShapeObject
        [ field "name" ShapeString
        , field "span" (ShapeNullable (ShapeNamed "SPAN"))
        , field "shape" (ShapeNamed "SHAPE")
        ]
    )
  ,
    ( "GOAL"
    , ShapeObject
        [ field "id" ShapeInteger
        , field "span" (ShapeNamed "SPAN")
        , field "shape" (ShapeNamed "SHAPE")
        , field "context" (ShapeArray (ShapeNamed "ENTRY"))
        ]
    )
  ]
 where
  location =
    ShapeNullable $
      ShapeObject [field "path" ShapeString, field "span" (ShapeNamed "SPAN")]
