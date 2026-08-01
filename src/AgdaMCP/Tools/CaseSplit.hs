{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.CaseSplit (
  caseSplitTool,
  Request (..),
  Response (..),
  Outcome (..),
  Edit (..),
  caseSplit,
  layoutClauses,
  renderResponse,
) where

import Agda.Syntax.Common (InteractionId (..))
import Data.Aeson (FromJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Map qualified as Map
import Data.Text (Text)
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )

import AgdaMCP.Interaction (Error, Span)
import AgdaMCP.Interaction.MakeCase (MakeCaseVariant)
import AgdaMCP.Tools.Internal (
  LoadId,
  LoadIdRefusal,
  ToolM,
  goalIdSchema,
  loadIdSchema,
  textToolHandle,
 )
import AgdaMCP.Tools.Load qualified as Load

caseSplitTool :: ToolHandler
caseSplitTool =
  toolHandler
    "case_split"
    ( Just ""
    -- TODO:
    -- "Case split an open goal in the currently loaded Agda file, replacing \
    -- \the clause the goal sits in with one clause per constructor. Pass the \
    -- \pattern variables to split on; an empty list instead introduces the \
    -- \clause's remaining arguments, or splits on the result if they are \
    -- \already introduced. A variable that is not currently in scope is \
    -- \revealed as a visible pattern rather than split on. The replacement is \
    -- \written to the file and the file is reloaded, so goal IDs are \
    -- \renumbered: use the goals in the result. Goal IDs are only meaningful \
    -- \against the load that issued them, so pass the `load_id` from that \
    -- \load result; an ID from an earlier load is refused."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [ ("load_id", loadIdSchema)
              , ("goal", goalIdSchema)
              ,
                ( "variables"
                , object
                    [ "type" .= ("array" :: Text)
                    , "items" .= object ["type" .= ("string" :: Text)]
                    , "description"
                        -- TODO: The empty-list case is the only thing that
                        -- selects argument introduction, so it has to be
                        -- spelled out here rather than left to be inferred.
                        .= ( "The pattern variables to split on, by name. An \
                             \empty list introduces the clause's remaining \
                             \arguments instead, or splits on the result if \
                             \they are already introduced." ::
                               Text
                           )
                    ]
                )
              ]
        )
        (Just ["load_id", "goal", "variables"])
    )
    (textToolHandle caseSplit renderResponse)

data Request = Request
  { caseSplitRequestLoadId :: LoadId
  , caseSplitRequestGoalId :: InteractionId
  , caseSplitRequestVariables :: [Text]
  {- ^ Empty introduces the clause's remaining arguments, or splits on the
  result if they are already introduced.
  -}
  }

data Response
  = -- The load id was refused.
    ResponseRefused LoadIdRefusal
  | {- The goal the split targeted, what happened, and the reload result that
    followed. -}
    ResponseCompleted InteractionId Outcome Load.Response
  deriving (Eq, Show)

data Outcome
  = OutcomeApplied Edit
  | -- No interaction point with that id in the current load.
    OutcomeUnknownGoal
  | {- The split itself failed, and it is the caller's fault. Either the goal was
    not in a clause, a named variable was unbound or not splittable, or the
    clause held a give since the last load (`checkClauseIsClean`,
    MakeCase.hs:463-466). -}
    OutcomeFailed Error
  | -- The file on disk no longer matches the source Agda checked.
    OutcomeFileChanged
  | -- Writing the edit failed. The payload is the rendered `IOException`.
    OutcomeIOError Text
  deriving (Eq, Show)

data Edit = Edit
  { editSpan :: Span
  {- ^ The clause extent that was replaced, from the start of the left-hand
  side to the end of the right-hand side.

  TODO: A `where` block is deliberately outside this extent, because Agda does
  not regenerate one (`makeAbstractClause` passes `A.noWhereDecls`). The
  consequence is that after splitting a clause carrying a `where`, the block
  ends up indented under the *last* generated clause and the earlier ones lose
  its bindings. Emacs behaves the same way, so this is not a bug to fix here,
  but it is a silent semantic change and the report should say so. Detecting
  it needs a wrapper-side signal (`clauseFullRange` differing from the fused
  LHS/RHS range); decide that before this tool's report shape is fixed.
  -}
  , editVariant :: MakeCaseVariant
  -- ^ How the clauses below were laid out. See `layoutClauses`.
  , editClauses :: [Text]
  {- ^ The replacement clauses as Agda produced them, one per element, before
  layout. The text actually written is `layoutClauses` applied to these, which
  is a pure function of what is already here, so it is not stored again; the
  file bytes are what the integration tests assert against.
  -}
  }
  deriving (Eq, Show)

-- Business logic

{- | TODO: The sequence, which is give's with a single edit instead of a batch:

1. `requireCurrentLoad` on `caseSplitRequestLoadId`, answering the canonical
   path and the `iSourceHash` fingerprint, before any command or IO.
2. Map `caseSplitRequestVariables` to the wrapper's `Split`:
   @maybe IntroduceArgumentsOrSplitResult SplitVariables (nonEmpty variables)@.
3. Run the `MakeCase` wrapper. `MakeCaseUnknownId` becomes `OutcomeUnknownGoal`,
   `MakeCaseFailed` becomes `OutcomeFailed` -- the same classification the goal
   tool applies, under a different policy, because here it is a user error and
   not a bug.
4. On success: read the source, check the fingerprint (`OutcomeFileChanged`),
   splice `layoutClauses` over `editSpan` at code-point offsets, commit
   atomically (`OutcomeIOError`).
5. Resync unconditionally -- applied, unknown, failed, file-changed, IO error
   alike -- and put the resulting `Load.Response` in `ResponseCompleted`. The
   reload issues the next `LoadId`.

TODO: Steps 4's read/fingerprint/splice/commit is give's, with @[(Span, Text)]@
of length one. Factor it out of the give tool rather than copying it; give is
the only other caller and the two must not drift.

TODO: Integration tests worth having, all of which need the live session:

* Splitting renumbers goals, so a goal id from before the split is refused
  against the new `load_id` -- the reason this tool takes one at all.
* A stale `load_id` is refused before anything runs: the file is untouched and
  no new id is issued.
* Every failing outcome still reloads and still reports a fresh `load_id`
  (the mandatory resync), and leaves the file byte-identical.
* Splitting the clause with a `where` block writes what the `where` hazard
  above describes -- pin the bytes, so the behavior is a decision on record
  rather than a surprise.
* An extended lambda splices inside the braces without disturbing the rest of
  the line.
* The fingerprint guard: touch the file after loading, then split, and expect
  `OutcomeFileChanged` with the file unchanged.
-}
caseSplit :: Request -> ToolM Response
caseSplit = error "un"

{- | Lay the replacement clauses out as text to splice over the clause extent.

TODO: The two forms are Emacs's (`agda2-make-case-action` and
`agda2-make-case-action-extendlam`, agda2-mode.el:916-953), except that we
splice over a span Agda gave us where Emacs has to guess one:

* `MakeCaseFunction`: one clause per line, each after the first indented to
  the column the replaced clause started at (@positionColumn (spanStart s) - 1@
  spaces -- the caller derives it, so this function stays free of `Span`).
* `MakeCaseExtendedLambda`: joined with @" ; "@ on one line, since the span
  sits inside an existing @λ { ... }@.

TODO: Unit tests, which need no session and should be written first. The
wrapper suite already pins the inputs: @["double zero = ?", "double (suc n) =
?"]@ at indent 0, and @["zero → ?", "(suc n) → ?"]@ for the extended lambda.
A single clause must come back unchanged in both forms.
-}
layoutClauses :: MakeCaseVariant -> Int -> [Text] -> Text
layoutClauses = error "un"

-- Request parsing

instance FromJSON Request where
  parseJSON = withObject "case_split arguments" $ \o ->
    Request
      <$> o .: "load_id"
      <*> (InteractionId <$> o .: "goal")
      <*> (o .: "variables" >>= traverse validateVariable)

{- | TODO: Reject the names that would silently select a different split than
the one asked for, since the wrapper re-encodes the list as the string Agda
splits with `words`:

* @"."@, which is Agda's ellipsis sentinel and would reach the branch this
  tool deliberately does not expose;
* a blank or whitespace-only name, which encodes to the empty string and
  would collide with the argument-introduction case;
* a name with embedded whitespace, which would silently become two variables.

TODO: Unit tests, no session needed: each of the three rejections, a plain
name accepted, and -- separately from this function -- that omitting
`variables` entirely is an argument error rather than an accidental
introduction, which is the whole reason the field is required.
-}
validateVariable :: Text -> Aeson.Parser Text
validateVariable = error "un"

-- Response rendering

-- TODO: Report the extent replaced, the clauses written, and the reload's
-- goals, which are the ids the caller must use from here. Inline exact, as
-- the other tools' renderers are.
renderResponse :: Response -> Text
renderResponse = error "un"
