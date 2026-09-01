{- | JSON encoders for the interaction model's types and for the refusals
several tools share, mirroring "AgdaMCP.Tools.Render". Functions rather than
instances so the interaction library stays free of aeson.
-}
module AgdaMCP.Tools.Json (
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

import Agda.Syntax.Common (InteractionId)
import Data.Aeson (Value)

import AgdaMCP.Interaction (
  ContextEntry,
  Error,
  GoalShape,
  HiddenMetavariable,
  NonFatalError,
  Position,
  Span,
  Warning,
 )
import AgdaMCP.Tools.LoadId (LoadIdRefusal)
import AgdaMCP.Tools.Source (SourceRefusal)

encodeGoalId :: InteractionId -> Value
encodeGoalId = error "not implemented"

encodePosition :: Position -> Value
encodePosition = error "not implemented"

encodeSpan :: Span -> Value
encodeSpan = error "not implemented"

encodeShape :: GoalShape -> Value
encodeShape = error "not implemented"

encodeContextEntry :: ContextEntry -> Value
encodeContextEntry = error "not implemented"

encodeError :: Error -> Value
encodeError = error "not implemented"

encodeWarning :: Warning -> Value
encodeWarning = error "not implemented"

encodeNonFatalError :: NonFatalError -> Value
encodeNonFatalError = error "not implemented"

encodeHiddenMetavariable :: HiddenMetavariable -> Value
encodeHiddenMetavariable = error "not implemented"

-- | The refusal by kind, with its rendered text as @message@.
encodeLoadIdRefusal :: LoadIdRefusal -> Value
encodeLoadIdRefusal = error "not implemented"

encodeSourceRefusal :: SourceRefusal -> Value
encodeSourceRefusal = error "not implemented"

encodeUnknownGoal :: InteractionId -> Value
encodeUnknownGoal = error "not implemented"
