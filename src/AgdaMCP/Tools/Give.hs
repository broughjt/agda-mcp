{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Give (
  BatchPosition (..),
  Edit (..),
  GiveOutcome (..),
  GiveResponse (..),
  RejectedGive (..),
  giveTool,
  renderGiveResponse,
) where

import Control.Applicative ((<|>))
import Control.Exception (
  Exception,
  Handler (..),
  IOException,
  catches,
  displayException,
 )
import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (toList)
import Data.List (find, nub, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LazyText
import MCP.Server (
  InputSchema (..),
  ProcessResult (..),
  ToolHandler,
  toolHandler,
  toolTextError,
  toolTextResult,
 )
import System.AtomicWrite.Writer.ByteString (atomicWriteFile)

import Agda.Interaction.Base (
  IOTCM' (..),
  Interaction' (Cmd_give),
  UseForce (WithoutForce),
 )
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  GiveResult (..),
  Response,
  Response_boot (..),
  Status (..),
 )
import Agda.Syntax.Common (InteractionId (..))
import Agda.Syntax.Common.Aspect (TokenBased (..))
import Agda.Syntax.Position (noRange)
import Agda.Syntax.Position qualified
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (
  Closure (clValue),
  HighlightingLevel (..),
  HighlightingMethod (..),
  InteractionError (NoSuchInteractionPoint),
  InteractionPoint (ipRange),
  TCErr (TypeError),
  TypeError (InteractionError),
  stInteractionPoints,
  useR,
 )
import Agda.TypeChecking.Monad.MetaVars (getInteractionRange)
import Agda.Utils.BiMap qualified as BiMap
import Agda.Utils.FileName (absolute)
import Agda.Utils.IO.UTF8 (ReadException, readTextFile)

import AgdaMCP.Position (
  Span,
  fileSpan,
  positionOffset,
  renderSpan,
  spanLength,
  spanStart,
  spanText,
  toSpan,
 )
import AgdaMCP.Session (
  ProtocolViolation (ProtocolViolation),
  SessionM,
  fromProtocolResult,
  liftTCM,
  runInteractionM,
 )
import AgdaMCP.Tools.Common (
  AgdaError (AgdaError),
  Warning (Warning),
  failedTail,
  goalName,
  resolveError,
  withSession,
 )
import AgdaMCP.Tools.Load (
  LoadRequest (..),
  LoadResponse (LoadFailed),
  load,
  renderLoadResponse,
 )

giveTool :: ToolHandler
giveTool =
  toolHandler
    "give"
    ( Just
        "Fill one or more goals in an Agda source file. Takes the file `path` and a non-empty list of `gives`, each containing a goal interaction ID and an expression. Gives are checked in order as an atomic batch: if one is rejected, subsequent gives are skipped and no source edits are written. Before writing, the server verifies that every recorded span still contains a hole. If the file changed, it refuses all edits. Every checked outcome is followed by a reload to resync. Interaction IDs may change, so use the goals in the fresh result. Successful gives write Agda’s elaborated, pretty-printed expressions, which may differ from the submitted text. The result reports both when they differ. Relative paths are resolved against the server process’s working directory. Prefer an absolute path when that directory may be ambiguous."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [
                ( "path"
                , object
                    [ "type" .= ("string" :: Text)
                    , "description"
                        .= ( "Path to an Agda source file, including literate Agda files. Relative paths are resolved against the server process's working directory." ::
                               Text
                           )
                    ]
                )
              ,
                ( "gives"
                , object
                    [ "type" .= ("array" :: Text)
                    , "description"
                        .= ( "A non-empty, all-or-nothing batch of goals to fill and expressions to fill them with" ::
                               Text
                           )
                    , "items"
                        .= object
                          [ "type" .= ("object" :: Text)
                          , "properties"
                              .= object
                                [ "goal"
                                    .= object
                                      [ "type" .= ("integer" :: Text)
                                      , "description"
                                          .= ("The target goal's interaction ID (`?N`) from a load result" :: Text)
                                      ]
                                , "expression"
                                    .= object
                                      [ "type" .= ("string" :: Text)
                                      , "description"
                                          .= ( "The Agda expression to check and fill the goal with" ::
                                                 Text
                                             )
                                      ]
                                ]
                          , "required" .= (["goal", "expression"] :: [Text])
                          ]
                    ]
                )
              ]
        )
        (Just ["path", "gives"])
    )
    ( either
        (pure . ProcessSuccess . toolTextError)
        ( fmap (ProcessSuccess . toolTextResult . (: []) . renderGiveResponse)
            . withSession
            . give
        )
        . parseGiveArguments
    )

data GiveRequest = GiveRequest FilePath [GiveItem]

-- Every `give` outcome ends by reloading (see `resync`), so the response
-- pairs the outcome with the fresh load result.
data GiveResponse = GiveResponse
  { giveOutcome :: GiveOutcome
  , giveReload :: LoadResponse
  }
  deriving (Show)

data GiveOutcome
  = -- Every give typechecked and its edit was spliced into the file.
    GiveApplied [Edit]
  | -- A give failed to typecheck, so the file was left untouched.
    GiveRejected RejectedGive
  | -- A give named an interaction ID that doesn't exist in the loaded file
    -- (Agda's `NoSuchInteractionPoint`), so no expression was ever checked.
    GiveUnknownGoal InteractionId BatchPosition
  | -- A goal's recorded span no longer holds a hole (the file changed on disk
    -- since it was loaded), so the edit was refused and nothing was written.
    GiveStale Edit
  | -- Reading or writing the file failed (deleted, permissions, disk full, ...).
    -- The write is atomic, so a failed write leaves the original
    -- intact. Carries the rendered I/O error.
    GiveIOError Text
  deriving (Show)

-- A single give Agda did not apply: either the goal's interaction ID doesn't
-- exist (nothing was checked), or checking the expression failed (with the
-- hole span when the interaction point exists, and the error).
data GiveFailure
  = UnknownGoal
  | GiveFailed (Maybe Span) AgdaError

-- A give that failed to typecheck: the goal, its hole span when the interaction
-- point still exists, its error, and where it sat in the batch.
data RejectedGive = RejectedGive
  { rejectedGoal :: InteractionId
  , rejectedSpan :: Maybe Span
  , rejectedError :: AgdaError
  , rejectedBatch :: BatchPosition
  }
  deriving (Show)

-- Where a failing give sat in its batch: earlier gives had already been
-- checked and are discarded by the resync reload; later gives were skipped
-- without being checked.
data BatchPosition = BatchPosition
  { batchDiscarded :: Int
  , batchSkipped :: Int
  }
  deriving (Show)

type GiveItem = (InteractionId, String)

-- TODO: Does Aeson have macros or derives that do this for us?
parseGiveArguments :: Maybe (Map Text Value) -> Either Text GiveRequest
parseGiveArguments arguments = do
  let arguments' = fromMaybe Map.empty arguments
  path <- case Map.lookup "path" arguments' of
    Just (Aeson.String p) -> Right (Text.unpack p)
    _ -> Left "Missing or invalid 'path' argument: expected a string"
  givesValue <-
    maybe
      (Left "Missing 'gives' argument: expected an array")
      Right
      (Map.lookup "gives" arguments')
  items <- parseGiveItems givesValue
  when (null items) $
    Left "The 'gives' array must contain at least one {goal, expression} object"
  let goals = [g | (g, _) <- items]
  when (length (nub goals) /= length goals) $
    Left "Duplicate goal ids in 'gives'; each goal may be given only once per call"
  pure (GiveRequest path items)
 where
  parseGiveItems :: Value -> Either Text [GiveItem]
  parseGiveItems (Aeson.Array items) = traverse parseGiveItem (toList items)
  parseGiveItems _ = Left "'gives' must be an array of {goal, expression} objects"

  parseGiveItem :: Value -> Either Text GiveItem
  parseGiveItem (Aeson.Object fields) = do
    goal <- case KeyMap.lookup "goal" fields of
      Just value -> case Aeson.fromJSON value of
        Aeson.Success n -> Right (InteractionId n)
        Aeson.Error _ -> Left "A 'gives' entry has a non-integer 'goal'"
      Nothing -> Left "A 'gives' entry is missing 'goal'"
    expression <- case KeyMap.lookup "expression" fields of
      Just (Aeson.String s) -> Right (Text.strip s)
      Just _ -> Left "A 'gives' entry has a non-string 'expression'"
      Nothing -> Left "A 'gives' entry is missing 'expression'"
    when (Text.null expression) $
      Left "A 'gives' entry has an empty 'expression'"
    pure (goal, (Text.unpack expression))
  parseGiveItem _ = Left "Each 'gives' entry must be a {goal, expression} object"

-- Run each give against the current loaded state in order, accumulating the
-- edits. The gives don't touch the file yet, so every edit's span stays valid
-- against the current source. If any give fails to typecheck, we stop, roll
-- back the in-flight gives by reloading the (untouched) file, and report the
-- failure. If each give is successful, we write the accumulated edits back to
-- disk.
give :: GiveRequest -> SessionM GiveResponse
give (GiveRequest path items) =
  runExceptT (traverse runGive (zip [0 :: Int ..] items))
    >>= either resync (\edits -> liftIO (commit edits) >>= resync)
 where
  runGive :: (Int, GiveItem) -> ExceptT GiveOutcome SessionM Edit
  runGive (index, (goal, expression)) =
    ExceptT $
      first (fromFailure goal (BatchPosition index (length items - index - 1)))
        <$> giveSingle path goal expression

  fromFailure :: InteractionId -> BatchPosition -> GiveFailure -> GiveOutcome
  fromFailure goal batch UnknownGoal = GiveUnknownGoal goal batch
  fromFailure goal batch (GiveFailed holeSpan err) =
    GiveRejected (RejectedGive goal holeSpan err batch)

  -- All gives succeeded. Read the source, confirm every recorded span still
  -- holds a hole (if not, the file change under us since the last load, so
  -- refuse rather than corrupt it), splice all the edits in, and write.
  commit :: [Edit] -> IO GiveOutcome
  commit edits =
    ( do
        -- Read the source with Agda's parser, so our edit offsets line up with
        -- its `posPos`.
        source <- LazyText.toStrict <$> readTextFile path
        -- We try to check that each of the `editSpan`s correspond to actual
        -- holes. See .scratch/GIVE_STALENESS.md.
        case find (not . spanIsHole . spanText source . editSpan) edits of
          Just stale -> pure (GiveStale stale)
          Nothing -> do
            -- Note: we write UTF-8 with LF endings regardless of platform.
            -- Agda's reader (`readTextFile`) normalizes to LF internally, so
            -- the hole span indices depend on this behavior. I just don't
            -- really care to think about what the defensibly correct solution
            -- is here, so I'm going to say "screw you Windows" and write LFs.
            atomicWriteFile path (encodeUtf8 (applyEdits source edits))
            pure (GiveApplied edits)
    )
      -- Catch `IOException`s (missing file, permissions, disk full) and
      -- `readTextFile`'s decoding `ReadException`.
      `catches` [ Handler (fromIOError :: IOException -> IO GiveOutcome)
                , Handler (fromIOError :: ReadException -> IO GiveOutcome)
                ]

  -- Every `give` outcome ends by reloading. The happy path uses this to report
  -- the fresh goals, while the sad paths discard any in-memory gives and sync
  -- Agda's state with the contents on disk.
  resync :: GiveOutcome -> SessionM GiveResponse
  resync outcome = GiveResponse outcome <$> load (LoadRequest path)

  fromIOError :: (Exception e) => e -> IO GiveOutcome
  fromIOError = pure . GiveIOError . Text.pack . displayException

giveSingle ::
  FilePath ->
  InteractionId ->
  String ->
  SessionM (Either GiveFailure Edit)
giveSingle path goal expression = do
  responses <-
    runInteractionM $
      const $
        -- TODO: Expose `UseForce` (the Emacs `C-u` give, skipping the safety
        -- checks) as an optional tool argument. Follow-up; wants its own
        -- thinking about when agents should force.
        IOTCM path None Direct (Cmd_give WithoutForce goal noRange expression)
  parsed <- fromProtocolResult $ parseGiveResponses goal responses
  resolved <-
    liftTCM $ resolveGive path goal (Text.pack expression) responses parsed
  fromProtocolResult resolved

-- An edit directive resulting from the application of a give command,
-- consisting of the interaction id, the hole's span, the submitted expression,
-- and the expression text elaborated by Agda and written to disk.
data Edit = Edit
  { editGoal :: InteractionId
  , editSpan :: Span
  , editSubmitted :: Text
  , editText :: Text
  }
  deriving (Show)

resolveGive ::
  FilePath ->
  InteractionId ->
  Text ->
  [Response] ->
  Either TCErr String ->
  TCM
    ( Either
        (ProtocolViolation Response)
        (Either GiveFailure Edit)
    )
resolveGive path goal _ _ (Left e)
  -- A give for a non-existent interaction ID fails `give_gen`'s first failable
  -- operation, `lookupInteractionPoint` (MetaVars.hs:638-640), with this
  -- dedicated constructor. Match it before rendering so it isn't mistakenly
  -- reported as an error in the submitted expression.
  | TypeError _ _ closure <- e
  , InteractionError (NoSuchInteractionPoint _) <- clValue closure =
      pure (Right (Left UnknownGoal))
  | otherwise = do
      path' <- liftIO (absolute path)
      interactionPoints <- useR stInteractionPoints
      let holeSpan = BiMap.lookup goal interactionPoints >>= fileSpan path' . ipRange
      err <- resolveError path' e
      pure (Right (Left (GiveFailed holeSpan err)))
resolveGive _ goal submitted responses (Right s) =
  maybe
    missing
    gave
    . Agda.Syntax.Position.rangeToInterval
    <$> getInteractionRange goal
 where
  gave interval =
    Right (Right (Edit goal (toSpan interval) submitted (Text.pack s)))
  missing = Left (ProtocolViolation "Cmd_give" responses)

-- TODO: Remove the `Text.strip`?
spanIsHole :: Text -> Bool
spanIsHole raw =
  stripped == "?"
    || ("{!" `Text.isPrefixOf` stripped && "!}" `Text.isSuffixOf` stripped)
 where
  stripped = Text.strip raw

-- Splice from the end of the file backwards so each edit's offsets stay valid:
-- an edit only shifts text after it, and holes never overlap.
applyEdits :: Text -> [Edit] -> Text
applyEdits source edits =
  foldl'
    splice
    source
    (sortBy (comparing (Down . positionOffset . spanStart . editSpan)) edits)
 where
  splice text edit =
    let (before, rest) = Text.splitAt (positionOffset (spanStart (editSpan edit))) text
        after = Text.drop (spanLength (editSpan edit)) rest
     in before <> editText edit <> after

{- The grammar of a Cmd_give response list, following the Agda 2.8.0 source:

exchange := prelude? given
          | prelude? failed

prelude := Status ClearRunningInfo ClearHighlighting RunningInfo*
        -- Appears only when the give targets a not-yet-loaded
        -- file. `runInteraction` executes an implicit cmd_load' with a no-op
        -- continuation (InteractionTop.hs:257-263, cmd_load':848-869).

given := GiveAction (goal, Give_String s) -- give_gen:1021; noRange => Give_String (:1010)
         Status                           -- give_gen ends with `Cmd_metas` => display_info (:1024)
         DisplayInfo (Info_AllGoalsWarnings)  -- (:1145-1146); discarded, superseded by the reload
         InteractionPoints                -- runInteraction:268-271 (updateInteractionPointsAfter Cmd_give)

failed := DisplayInfo (Info_Error)       -- handleErr:216-242, as in matchLoad
          JumpToError?                   -- with noRange, typically absent
          HighlightingInfo
          Status                         -- hardcoded sChecked=False (:239)
-}
parseGiveResponses ::
  InteractionId ->
  [Response] ->
  Either (ProtocolViolation Response) (Either TCErr String)
parseGiveResponses goal responses = maybe (Left violation) Right (exchange responses)
 where
  violation = ProtocolViolation "Cmd_give" responses

  exchange
    ( Resp_Status status : Resp_ClearRunningInfo
        : Resp_ClearHighlighting NotOnlyTokenBased
        : rest
      )
      | not (sChecked status) = afterPrelude rest
  exchange rest = afterPrelude rest

  afterPrelude (Resp_RunningInfo {} : rest) = afterPrelude rest
  afterPrelude rest = given rest <|> failed rest

  given
    ( Resp_GiveAction goal' (Give_String s)
        : Resp_Status _
        : Resp_DisplayInfo (Info_AllGoalsWarnings _ _)
        : [Resp_InteractionPoints _]
      )
      | goal' == goal = Just (Right s)
  given _ = Nothing

  failed = failedTail Left

renderGiveResponse :: GiveResponse -> Text
-- When a give was rejected by its implicit load, the resync reload fails with
-- the same error. Repeating it verbatim is redundant, so we detect this case
-- and more concise version.
renderGiveResponse (GiveResponse outcome@(GiveRejected rejected) (LoadFailed reloadError))
  | rejectedError rejected == reloadError =
      renderGiveOutcome outcome <> " load failed with the same error."
renderGiveResponse (GiveResponse outcome reloaded) =
  renderGiveOutcome outcome <> "\n\n" <> renderLoadResponse reloaded

renderGiveOutcome :: GiveOutcome -> Text
renderGiveOutcome (GiveApplied edits) =
  "Applied "
    <> count
    <> (if length edits == 1 then " give:\n\n" else " gives:\n\n")
    <> Text.intercalate "\n" (map renderAppliedEdit edits)
    <> "\n\nFile updated and reloaded; interaction IDs may have changed."
 where
  count = Text.pack (show (length edits))
renderGiveOutcome (GiveRejected (RejectedGive goal holeSpan err batch)) =
  "Give rejected for "
    <> goalName goal
    <> maybe "." (\s -> " (at " <> renderSpan s <> ").") holeSpan
    <> "\n\n"
    <> renderRejectedError err
    <> "\n\n"
    <> renderUnapplied batch
    <> " Reloaded to resync:"
renderGiveOutcome (GiveUnknownGoal goal batch) =
  "No such goal "
    <> goalName goal
    <> " in the loaded file. Goal IDs renumber after every edit or reload; \
       \use the IDs from the fresh list below.\n\n"
    <> renderUnapplied batch
    <> " Reloaded to resync:"
renderGiveOutcome (GiveStale edit) =
  "Edit refused for "
    <> goalName (editGoal edit)
    <> " at "
    <> renderSpan (editSpan edit)
    <> ".\n\nThe target no longer contains a hole, so the file may have changed \
       \since it was loaded. No changes were made.\n\nReloaded to resync:"
renderGiveOutcome (GiveIOError err) =
  "Could not access the file on disk:\n\n"
    <> err
    <> "\n\nNo changes were written.\n\nReloaded to resync:"

-- The span labels read "was at", since they point to the hole in the file as it
-- was before this call's edits were applied.
renderAppliedEdit :: Edit -> Text
renderAppliedEdit edit
  | submitted == written =
      header
        <> if "\n" `Text.isInfixOf` written
          then
            " :=\n"
              <> indent 2 written
              <> "\n  (was at "
              <> renderSpan (editSpan edit)
              <> ")"
          else
            " := "
              <> written
              <> " (was at "
              <> renderSpan (editSpan edit)
              <> ")"
  | otherwise =
      Text.intercalate
        "\n"
        [ header <> ":"
        , renderExpression "submitted" submitted
        , renderExpression "written" written
        , "  (was at " <> renderSpan (editSpan edit) <> ")"
        ]
 where
  header = goalName (editGoal edit)
  submitted = editSubmitted edit
  written = editText edit

renderExpression :: Text -> Text -> Text
renderExpression label expression
  | "\n" `Text.isInfixOf` expression =
      "  " <> label <> ":\n" <> indent 4 expression
  | otherwise =
      "  "
        <> label
        <> ":"
        <> Text.replicate (10 - Text.length label) " "
        <> expression

indent :: Int -> Text -> Text
indent width text =
  let prefix = Text.replicate width " "
   in prefix <> Text.replace "\n" ("\n" <> prefix) text

renderRejectedError :: AgdaError -> Text
renderRejectedError (AgdaError message errorSpan warnings) =
  Text.intercalate "\n\n" $
    [ maybe
        "Expression error (locations are relative to the submitted expression):"
        (\s -> "Agda error at " <> renderSpan s <> ":")
        errorSpan
    , message
    ]
      <> case warnings of
        [] -> []
        _ -> ["Warnings:\n\n" <> Text.intercalate "\n" [w | Warning (_, w) <- warnings]]

-- e.g. "No file changes were made; 1 earlier give in this call was discarded
-- and 2 later gives were skipped."
renderUnapplied :: BatchPosition -> Text
renderUnapplied (BatchPosition 0 0) = "No file changes were made."
renderUnapplied (BatchPosition discarded skipped) =
  "No file changes were made; "
    <> Text.intercalate " and " (discardedClause <> skippedClause)
    <> "."
 where
  -- "in this call" reads once, on the first clause present.
  discardedClause =
    [ gives discarded "earlier" <> " in this call " <> was discarded <> " discarded"
    | discarded > 0
    ]
  skippedClause =
    [ gives skipped "later"
        <> (if discarded > 0 then " " else " in this call ")
        <> was skipped
        <> " skipped"
    | skipped > 0
    ]
  gives n position =
    Text.pack (show n)
      <> " "
      <> position
      <> (if n == 1 then " give" else " gives")
  was n = if n == 1 then "was" else "were" :: Text
