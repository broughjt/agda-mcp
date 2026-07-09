{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Give (giveTool) where

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
import Agda.Interaction.EmacsTop (showInfoError)
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  GiveResult (..),
  Info_Error,
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
  HighlightingLevel (..),
  HighlightingMethod (..),
 )
import Agda.TypeChecking.Monad.MetaVars (getInteractionRange)
import Agda.Utils.IO.UTF8 (ReadException, readTextFile)

import AgdaMCP.Position (
  Span,
  positionOffset,
  renderSpan,
  spanLength,
  spanStart,
  spanText,
  toSpan,
 )
import AgdaMCP.Tools.Common (failedTail, runCommand)
import AgdaMCP.Tools.Load (
  LoadRequest (..),
  LoadResponse,
  load,
  renderLoadResponse,
 )
import AgdaMCP.Worker (
  Command (..),
  ProtocolViolation (ProtocolViolation),
  Worker,
 )

giveTool :: Worker -> ToolHandler
giveTool worker =
  toolHandler
    "agda_give"
    ( Just
        -- TODO:
        "Fill one or more goals of a loaded Agda file with expressions. Takes \
        \the file `path` and a list of `gives`, each a `goal` (interaction id, \
        \as reported by agda_load) and an `expression`. All gives are checked \
        \against the currently loaded state, then applied to the file together \
        \and the file is reloaded. If any give fails to typecheck the whole \
        \call is a no-op (the file is left unchanged). After a successful call \
        \the interaction ids are renumbered by the reload, so read the new \
        \goals from the result before giving again."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [
                ( "path"
                , object
                    [ "type" .= ("string" :: Text)
                    , "description" .= ("Path to a .agda file" :: Text)
                    ]
                )
              ,
                ( "gives"
                , object
                    [ "type" .= ("array" :: Text)
                    , "description"
                        .= ("The goals to fill and the expressions to fill them with" :: Text)
                    , "items"
                        .= object
                          [ "type" .= ("object" :: Text)
                          , "properties"
                              .= object
                                [ "goal"
                                    .= object
                                      [ "type" .= ("integer" :: Text)
                                      , "description" .= ("The interaction id of the goal" :: Text)
                                      ]
                                , "expression"
                                    .= object
                                      [ "type" .= ("string" :: Text)
                                      , "description" .= ("The expression to give" :: Text)
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
        ( liftIO
            . fmap (ProcessSuccess . toolTextResult . (: []) . renderGiveResponse)
            . give worker
        )
        . parseGiveArguments
    )

data GiveRequest = GiveRequest FilePath [GiveItem]

data GiveResponse
  = -- Every give typechecked and its edit was spliced into the file.
    GiveApplied [Edit] LoadResponse
  | -- A give failed to typecheck, so the file was left untouched. Carries the
    -- goal that failed, its rendered error, and how many earlier gives in the
    -- same call were discarded.
    GiveRejected InteractionId Text Int LoadResponse
  | -- A goal's recorded span no longer holds a hole (the file changed on disk
    -- since it was loaded), so the edit was refused and nothing was written.
    GiveStale Edit LoadResponse
  | -- Reading or writing the file failed (deleted, permissions, disk full, ...).
    -- Nothing was written -- the write is atomic, so a failed write leaves the
    -- original intact. Carries the rendered I/O error.
    GiveIOError Text LoadResponse
  deriving (Show)

-- TODO: Merge with or pull out of GiveRejected
data RejectedGive = RejectedGive InteractionId Text Int

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
give :: Worker -> GiveRequest -> IO GiveResponse
give worker (GiveRequest path items) =
  runExceptT (traverse runGive (zip [0 :: Int ..] items))
    >>= either reject commit
 where
  runGive :: (Int, GiveItem) -> ExceptT RejectedGive IO Edit
  runGive (priorCount, (goal, expression)) =
    ExceptT $
      first (flip (RejectedGive goal) priorCount)
        <$> runCommand worker (giveCommand path goal expression)

  -- TODO: Consequence of TODO above saying to merge `RejectedGive` with `GiveRejected`.
  reject :: RejectedGive -> IO GiveResponse
  reject (RejectedGive goal err priorCount) =
    resync (GiveRejected goal err priorCount)

  -- All gives succeeded. Read the source, confirm every recorded span still
  -- holds a hole (if not, the file change under us since the last load, so
  -- refuse rather than corrupt it), splice all the edits in, write, and reload.
  commit :: [Edit] -> IO GiveResponse
  commit edits =
    ( do
        -- Read the source with Agda's parser, so our edit offsets line up with
        -- its `posPos`.
        source <- LazyText.toStrict <$> readTextFile path
        -- We try to check that each of the `editSpan`s correspond to actual
        -- holes. See .scratch/GIVE_STALENESS.md.
        case find (not . spanIsHole . spanText source . editSpan) edits of
          Just stale -> resync (GiveStale stale)
          Nothing -> do
            -- Note: we write UTF-8 with LF endings regardless of platform.
            -- Agda's reader (`readTextFile`) normalizes to LF internally, so
            -- the hole span indices depend on this behavior. I just don't
            -- really care to think about what the defensibly correct solution
            -- is here, so I'm going to say "screw you Windows" and write LFs.
            atomicWriteFile path (encodeUtf8 (applyEdits source edits))
            resync (GiveApplied edits)
    )
      -- Catch `IOException`s (missing file, permissions, disk full) and
      -- `readTextFile`'s decoding `ReadException`.
      `catches` [ Handler (fromIOError :: IOException -> IO GiveResponse)
                , Handler (fromIOError :: ReadException -> IO GiveResponse)
                ]

  -- Every `give` outcome ends by reloading. The happy path uses this to report
  -- the fresh goals, while the sad paths discard any in-memory gives and sync
  -- Agda's state with the contents on disk.
  resync :: (LoadResponse -> a) -> IO a
  resync f = f <$> load worker (LoadRequest path)

  fromIOError :: (Exception e) => e -> IO GiveResponse
  fromIOError = resync . GiveIOError . Text.pack . displayException

-- Either a rendered type error (the give failed) or the edit to splice: the
-- hole's span and the text Agda elaborated the expression to.
giveCommand ::
  FilePath -> InteractionId -> String -> Command (Either Text Edit)
giveCommand path goal expression =
  Command
    { commandIOTCM =
        IOTCM path None Direct (Cmd_give WithoutForce goal noRange expression)
    , commandParse = \responses ->
        either
          (pure . Left)
          (resolveGive goal responses)
          (parseGiveResponses goal responses)
    }

-- An edit directive resulting from the application of a give command,
-- consisting of the interaction id, the hole's span, and the expression text
-- elaborated by Agda.
data Edit = Edit
  { editGoal :: InteractionId
  , editSpan :: Span
  , editText :: Text
  }
  deriving (Show)

resolveGive ::
  InteractionId ->
  [Response] ->
  Either Info_Error String ->
  TCM (Either (ProtocolViolation Response) (Either Text Edit))
resolveGive _ _ (Left e) = Right . Left . Text.pack <$> showInfoError e
resolveGive goal responses (Right s) =
  maybe
    missing
    gave
    . Agda.Syntax.Position.rangeToInterval
    <$> getInteractionRange goal
 where
  gave interval = Right (Right (Edit goal (toSpan interval) (Text.pack s)))
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
  Either (ProtocolViolation Response) (Either Info_Error String)
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
renderGiveResponse (GiveApplied edits reloaded) =
  "Gave "
    <> Text.pack (show (length edits))
    <> " goal(s):\n"
    <> Text.concat
      [ "  ?"
          <> Text.pack (show (interactionId (editGoal e)))
          <> " := "
          <> editText e
          <> "\n"
      | e <- edits
      ]
    <> "The file was updated on disk and reloaded; interaction ids may have changed.\n\n"
    <> renderLoadResponse reloaded
renderGiveResponse (GiveRejected goal err priorCount reloaded) =
  "Give failed for goal ?"
    <> Text.pack (show (interactionId goal))
    <> ":\n"
    <> err
    <> "\n\n"
    <> ( if priorCount > 0
           then
             "The file was left unchanged; the "
               <> Text.pack (show priorCount)
               <> " earlier give(s) in this call were discarded. "
           else "The file was left unchanged. "
       )
    <> "Reloaded to resync:\n\n"
    <> renderLoadResponse reloaded
renderGiveResponse (GiveStale edit reloaded) =
  "Refused to edit: goal ?"
    <> Text.pack (show (interactionId (editGoal edit)))
    <> " no longer points at a hole (expected `?` or `{! !}` at "
    <> renderSpan (editSpan edit)
    <> "). The file likely changed on disk since it was loaded. No changes were \
       \made; reloaded to resync:\n\n"
    <> renderLoadResponse reloaded
renderGiveResponse (GiveIOError err reloaded) =
  "Could not access the file on disk:\n"
    <> err
    <> "\n\nNo changes were written. Reloaded to resync:\n\n"
    <> renderLoadResponse reloaded
