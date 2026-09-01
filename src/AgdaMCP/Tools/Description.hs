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
  timeoutField,
  noCurrentLoad,
  staleLoadId,
  foreignLoadId,
  sourceChanged,
  sourceUnreadable,
  unknownGoal,
  invalidArguments,
  timedOut,
  namedShapes,
) where

import Data.Text (Text)

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
parameterDescription = error "not implemented"

-- | How the tools fit together, for the MCP server's instructions and the CLI's help.
overview :: Text
overview = error "not implemented"

-- Fields shared by several tools

loadIdField :: Field
loadIdField = error "not implemented"

goalIdField :: Field
goalIdField = error "not implemented"

normalizationField :: Field
normalizationField = error "not implemented"

-- | Takes the tool's default in seconds.
timeoutField :: Int -> Field
timeoutField = error "not implemented"

-- Refusals shared by several tools

noCurrentLoad, staleLoadId, foreignLoadId :: Variant
noCurrentLoad = error "not implemented"
staleLoadId = error "not implemented"
foreignLoadId = error "not implemented"

sourceChanged, sourceUnreadable, unknownGoal :: Variant
sourceChanged = error "not implemented"
sourceUnreadable = error "not implemented"
unknownGoal = error "not implemented"

-- | The answers the adapter gives before a tool runs.
invalidArguments, timedOut :: Variant
invalidArguments = error "not implemented"
timedOut = error "not implemented"

-- | The leaves the tool values share, by the names their descriptions use.
namedShapes :: [(Text, Shape)]
namedShapes = error "not implemented"
