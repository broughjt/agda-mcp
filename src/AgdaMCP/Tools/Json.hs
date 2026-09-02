{-# LANGUAGE OverloadedStrings #-}

{- | JSON encoders for the interaction model's types and for the refusals
several tools share, mirroring "AgdaMCP.Tools.Render". Functions rather than
instances so the interaction library stays free of aeson.
-}
module AgdaMCP.Tools.Json (
  kinded,
  encodeGoalId,
  encodePosition,
  encodeSpan,
  encodeShape,
  encodeContextEntry,
  encodeError,
  encodeWarning,
  encodeNonFatalError,
  encodeHiddenMetavariable,
  encodeLoadIdRefusal,
  encodeSourceRefusal,
  encodeUnknownGoal,
) where

import Agda.Syntax.Common (InteractionId (..))
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Types (Pair)
import Data.Text (Text)

import AgdaMCP.Interaction (
  ContextEntry (..),
  Error (..),
  GoalShape (..),
  HiddenMetavariable (..),
  NonFatalError (..),
  Position (..),
  Span (..),
  Warning (..),
 )
import AgdaMCP.Tools.LoadId (
  LoadIdRefusal (..),
  renderLoadId,
  renderLoadIdRefusal,
 )
import AgdaMCP.Tools.Render (renderSourceRefusal, renderUnknownGoal)
import AgdaMCP.Tools.Source (SourceRefusal (..))

-- | One constructor of a sum: its @kind@, then its fields beside it.
kinded :: Text -> [Pair] -> Value
kinded kind fields = object $ ("kind" .= kind) : fields

encodeGoalId :: InteractionId -> Value
encodeGoalId = toJSON . interactionId

encodePosition :: Position -> Value
encodePosition (Position offset line column) =
  object ["offset" .= offset, "line" .= line, "column" .= column]

encodeSpan :: Span -> Value
encodeSpan (Span start end) =
  object ["start" .= encodePosition start, "end" .= encodePosition end]

encodeShape :: GoalShape -> Value
encodeShape (GoalOfType t) = kinded "of_type" ["type" .= t]
encodeShape GoalSort = kinded "sort" []

encodeContextEntry :: ContextEntry -> Value
encodeContextEntry entry =
  object
    [ "original_name" .= contextEntryOriginalName entry
    , "reified_name" .= contextEntryReifiedName entry
    , "original_in_scope" .= contextEntryOriginalInScope entry
    , "reified_in_scope" .= contextEntryReifiedInScope entry
    , "type" .= contextEntryType entry
    , "let_value" .= contextEntryLetValue entry
    , "is_instance" .= contextEntryIsInstance entry
    , "cohesion" .= contextEntryCohesion entry
    , "polarity" .= contextEntryPolarity entry
    , "erased" .= contextEntryErased entry
    , "relevance" .= contextEntryRelevance entry
    ]

encodeLocation :: Maybe (FilePath, Span) -> Value
encodeLocation Nothing = Null
encodeLocation (Just (path, s)) = object ["path" .= path, "span" .= encodeSpan s]

encodeError :: Error -> Value
encodeError e =
  object
    [ "message" .= errorMessage e
    , "location" .= encodeLocation (errorPathSpan e)
    , "warnings" .= map encodeWarning (errorWarnings e)
    ]

encodeWarning :: Warning -> Value
encodeWarning (Warning (location, message)) =
  object ["message" .= message, "location" .= encodeLocation location]

encodeNonFatalError :: NonFatalError -> Value
encodeNonFatalError (NonFatalError (location, message)) =
  object ["message" .= message, "location" .= encodeLocation location]

encodeHiddenMetavariable :: HiddenMetavariable -> Value
encodeHiddenMetavariable metavariable =
  object
    [ "name" .= hiddenMetavariableName metavariable
    , "span" .= fmap encodeSpan (hiddenMetavariableSpan metavariable)
    , "shape" .= encodeShape (hiddenMetavariableShape metavariable)
    ]

-- | The refusal by kind, with its rendered text as @message@.
encodeLoadIdRefusal :: LoadIdRefusal -> Value
encodeLoadIdRefusal refusal =
  kinded kind $ fields <> ["message" .= renderLoadIdRefusal refusal]
 where
  (kind, fields) = case refusal of
    NoCurrentLoad -> ("no_current_load", [])
    StaleLoadId current -> ("stale_load_id", ["current_load_id" .= renderLoadId current])
    ForeignLoadId -> ("foreign_load_id", [])

encodeSourceRefusal :: SourceRefusal -> Value
encodeSourceRefusal refusal =
  kinded kind $ fields <> ["message" .= renderSourceRefusal refusal]
 where
  (kind, fields) = case refusal of
    RefusalChanged -> ("source_changed", [])
    RefusalUnreadable detail -> ("source_unreadable", ["detail" .= detail])

encodeUnknownGoal :: InteractionId -> Value
encodeUnknownGoal unknownId =
  kinded
    "unknown_goal"
    [ "goal" .= encodeGoalId unknownId
    , "message" .= renderUnknownGoal unknownId
    ]
