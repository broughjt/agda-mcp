{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Give (
  BatchPosition (..),
  Edit (..),
  GiveBug (..),
  GiveItem,
  GiveOutcome (..),
  GiveRequest (..),
  GiveResponse (..),
  GiveRejection (..),
  give,
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
  throwIO,
 )
import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, lift)
import Data.Aeson (FromJSON (parseJSON), Value, object, withObject, (.:), (.=))
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (toList)
import Data.List (find, nub, sortBy)
import Data.Map qualified as Map
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LazyText
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )
import System.AtomicWrite.Writer.ByteString (atomicWriteFile)

import Agda.Interaction.Base (
  CommandState (theCurrentFile),
  CurrentFile (currentFileModule, currentFilePath),
  IOTCM' (..),
  Interaction' (Cmd_give),
  UseForce (WithoutForce),
 )
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  GiveResult (..),
  Response,
  Response_boot (..),
 )
import Agda.Syntax.Common (InteractionId (..))
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Position (noRange)
import Agda.Syntax.Position qualified
import Agda.TypeChecking.Monad (TCM)
import Agda.TypeChecking.Monad.Base (
  Closure (clValue),
  HighlightingLevel (..),
  HighlightingMethod (..),
  InteractionError (NoSuchInteractionPoint),
  InteractionPoint (ipRange),
  Interface (iSourceHash),
  ModuleInfo (miInterface),
  TCErr (IOException, TypeError),
  TypeError (InteractionError),
  stInteractionPoints,
  useR,
 )
import Agda.TypeChecking.Monad.Imports (getVisitedModule)
import Agda.TypeChecking.Monad.MetaVars (getInteractionRange)
import Agda.Utils.BiMap qualified as BiMap
import Agda.Utils.FileName (absolute, filePath)
import Agda.Utils.Hash (Hash, hashText)
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
import AgdaMCP.ResponseProtocol (
  AgdaResponseMismatch (AgdaResponseMismatch),
  throwMismatch,
 )
import AgdaMCP.Session (runInteractionM)
import AgdaMCP.Tools.Common (
  AgdaError (AgdaError),
  Warning (Warning),
  failedTail,
  renderGoalId,
  resolveError,
  targetIsLoaded,
  textToolHandle,
 )
import AgdaMCP.Tools.Load (
  LoadRequest (..),
  LoadResponse,
  load,
  renderLoadResponse,
 )
import Data.Bitraversable (bitraverse)

giveTool :: ToolHandler
giveTool =
  toolHandler
    "give"
    ( Just
        "Fill one or more goals in an Agda source file. Takes the file `path` and a non-empty list of `gives`, each consisting of a goal interaction ID and an expression. The file must be the currently loaded file. Agda tracks goals for one file at a time, so goal interaction IDs are only valid for the most recently loaded file, and a give against any other file is refused and returns that file's fresh load result to give against instead. Gives are checked in order as an atomic batch: if one is rejected, subsequent gives are skipped and no source edits are written. Before writing, the server verifies that the file on disk still matches the source Agda checked; if it changed, all edits are refused. Every checked outcome is followed by a reload to resync. Interaction IDs may change, so use the goals in the fresh result. Successful gives write Agda’s elaborated, pretty-printed expressions, which may differ from the submitted text. The result reports both when they differ. Relative paths are resolved against the server process’s working directory. Prefer an absolute path when that directory may be ambiguous."
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
                        .= ( "Path to an Agda source file (.agda, but also \
                             \literate formats such as .lagda.md, .lagda.tex, \
                             \.lagda.typ, etc). Relative paths are resolved \
                             \against the server process's working directory." ::
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
    (textToolHandle give renderGiveResponse)

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
    GiveRejected GiveRejection
  | -- A give named an interaction ID that doesn't exist in the loaded file
    -- (Agda's `NoSuchInteractionPoint`), so the current and subsequent give
    -- expressions were not checked.
    GiveUnknownGoal InteractionId BatchPosition
  | -- The target file is not the currently loaded file. Goal interaction IDs
    -- are only valid for the most recently loaded file, and loading a new file
    -- destroys the previous file's interaction points.
    GiveNotLoaded
  | -- The on-disk source no longer matches the source Agda checked (the file
    -- changed since it was loaded), so the recorded goal spans are unreliable
    -- and all edits were refused.
    GiveFileChanged
  | -- Reading or writing the file failed (for example, because the file was
    -- deleted, permissions changed, or the disk was full).
    GiveIOError Text
  deriving (Show)

-- Internal failures of the pre-write staleness guard. Both bugs in agda-mcp.
-- Thrown and caught nowhere so that the process dies with debug output on
-- stderr.
data GiveBug
  = -- After a batch of successful gives, locating the loaded source's
    -- fingerprint broke.
    FingerprintUnavailable FilePath String
  | -- The on-disk source is exactly the text Agda checked (equal hashes of the
    -- normalized text), and yet the recorded goal span does not contain a
    -- hole. In this case, our mental model of Agda's offsets or text
    -- normalization is wrong.
    SpanNotHole FilePath InteractionId Span Text Hash
  deriving (Show)

instance Exception GiveBug

-- The reason a single give was not applied. The goal may not exist, checking
-- the expression may have failed, or an environmental I/O error may have
-- interrupted Agda after it accepted the expression but before the command
-- completed.
data GiveFailure
  = UnknownGoal
  | GiveFailed (Maybe Span) AgdaError
  | GiveIOFailed Text

-- Information about why a give failed to typecheck, including the goal, the
-- goal's span (when the interaction point still exists), its error, and its
-- position in the batch of gives.
data GiveRejection = GiveRejection
  { rejectedGoal :: InteractionId
  , rejectedSpan :: Maybe Span
  , rejectedError :: AgdaError
  , rejectedBatch :: BatchPosition
  }
  deriving (Show)

-- Index of the failed give and length of the batch of gives.
data BatchPosition = BatchPosition
  { batchIndex :: Int
  , batchLength :: Int
  }
  deriving (Show)

type GiveItem = (InteractionId, String)

instance FromJSON GiveRequest where
  parseJSON = withObject "give arguments" $ \o -> do
    path <- o .: "path"
    items <- Aeson.explicitParseField parseGiveItems o "gives"
    when (null items) $
      fail "The 'gives' array must contain at least one {goal, expression} object"
    let goals = [g | (g, _) <- items]
    when (length (nub goals) /= length goals) $
      fail "Duplicate goal ids in 'gives'; each goal may be given only once per call"
    pure (GiveRequest path items)

-- `Aeson.listParser` doesn't record each element's index in the error path, so
-- we annotate it ourselves.
parseGiveItems :: Value -> Aeson.Parser [GiveItem]
parseGiveItems = Aeson.withArray "gives" $ \items ->
  traverse
    (\(i, item) -> parseGiveItem item Aeson.<?> Aeson.Index i)
    (zip [0 ..] (toList items))

parseGiveItem :: Value -> Aeson.Parser GiveItem
parseGiveItem = withObject "give" $ \o -> do
  goal <- InteractionId <$> o .: "goal"
  expression <- Text.strip <$> o .: "expression"
  when (Text.null expression) $
    fail "'expression' is empty or whitespace-only"
  pure (goal, Text.unpack expression)

-- Run each give against the current loaded state in order, accumulating the
-- edits. The gives don't touch the file yet, so every edit's span stays valid
-- against the current source. If any give fails to type check, we stop, roll
-- back the in-flight gives by reloading the (untouched) file, and report the
-- failure. If each give is successful, we write the accumulated edits back to
-- disk.
give :: GiveRequest -> CommandM GiveResponse
give (GiveRequest path items) = do
  -- Without the loaded-file check Agda would load the file implicitly and
  -- interpret the IDs against fresh interaction points the caller never saw.
  loaded <- targetIsLoaded path
  outcome <-
    if loaded
      then
        runExceptT (traverse runGive (zip [0 :: Int ..] items))
          >>= either pure checkedCommit
      else pure GiveNotLoaded
  resync outcome
 where
  -- All gives succeeded; fetch the loaded source's fingerprint and commit the
  -- edits.
  checkedCommit :: [Edit] -> CommandM GiveOutcome
  checkedCommit edits = loadedSourceHash path >>= liftIO . flip commit edits

  runGive :: (Int, GiveItem) -> ExceptT GiveOutcome CommandM Edit
  runGive (index, (goal, expression)) =
    ExceptT $
      first (fromFailure goal (BatchPosition index (length items)))
        <$> giveSingle path goal expression

  fromFailure :: InteractionId -> BatchPosition -> GiveFailure -> GiveOutcome
  fromFailure goal batch UnknownGoal = GiveUnknownGoal goal batch
  fromFailure goal batch (GiveFailed holeSpan err) =
    GiveRejected (GiveRejection goal holeSpan err batch)
  fromFailure _ _ (GiveIOFailed message) = GiveIOError message

  -- All gives succeeded. Read the source and confirm it is still the source
  -- Agda checked (equal hashes of the normalized text). If not, the recorded
  -- spans are unreliable, so refuse rather than risk corrupting the file. When
  -- the hashes match, every span must still hold a hole. If this is not the
  -- case, this is an agda-mcp bug and we die loudly.
  commit :: Hash -> [Edit] -> IO GiveOutcome
  commit expected edits =
    ( do
        -- Read the source with Agda's parser, so our edit offsets line up with
        -- its `posPos` and the hash matches `iSourceHash`'s input.
        lazySource <- readTextFile path
        let source = LazyText.toStrict lazySource
        if hashText lazySource /= expected
          then pure GiveFileChanged
          else case find (not . spanIsHole . spanText source . editSpan) edits of
            Just bad ->
              throwIO $
                SpanNotHole
                  path
                  (editGoal bad)
                  (editSpan bad)
                  (spanText source (editSpan bad))
                  expected
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

  -- Every `give` outcome ends by loading the file. The happy path uses this
  -- to report the fresh goals, the sad paths discard any in-memory gives and
  -- sync Agda's state with the contents on disk, and `GiveNotLoaded` gets the
  -- initial load whose goal IDs the caller was missing.
  resync :: GiveOutcome -> CommandM GiveResponse
  resync outcome = GiveResponse outcome <$> load (LoadRequest path)

  fromIOError :: (Exception e) => e -> IO GiveOutcome
  fromIOError = pure . GiveIOError . Text.pack . displayException

-- Obtain the hash of the normalized source text for the currently loaded file
-- that Agda checked. Gives only run after `targetIsLoaded` confirmed the
-- target is the loaded current file, so every broken link here is an agda-mcp
-- bug.
loadedSourceHash :: FilePath -> CommandM Hash
loadedSourceHash path = do
  -- `parseSource` reads the file with `readTextFile` (Imports.hs:165) and
  -- hashes exactly that text into the interface (`iSourceHash = hashText
  -- source`, Imports.hs:1411). `visitModule` records the interface even when
  -- the module has warnings such as open holes (Import.hs:467, warnings only
  -- skip `storeDecodedModule`).
  path' <- liftIO $ absolute path
  current <- gets theCurrentFile
  case current of
    Nothing -> bug "no file is loaded (`theCurrentFile` is `Nothing`)"
    Just file
      | currentFilePath file /= path' ->
          bug ("the loaded file is " <> filePath (currentFilePath file))
      | otherwise -> do
          visited <- lift $ getVisitedModule $ currentFileModule file
          case visited of
            Nothing ->
              bug $
                "module "
                  <> prettyShow (currentFileModule file)
                  <> " has no visited interface"
            Just moduleInfo -> pure $ iSourceHash $ miInterface moduleInfo
 where
  bug :: String -> CommandM a
  bug = liftIO . throwIO . FingerprintUnavailable path

giveSingle ::
  FilePath ->
  InteractionId ->
  String ->
  CommandM (Either GiveFailure Edit)
giveSingle path goal expression = do
  responses <-
    runInteractionM $
      const $
        -- TODO: Expose `UseForce` (the Emacs `C-u` give, skipping the safety
        -- checks) as an optional tool argument. Follow-up; wants its own
        -- thinking about when agents should force.
        IOTCM path None Direct (Cmd_give WithoutForce goal noRange expression)
  parsed <- lift $ either throwMismatch pure $ parseGiveResponses goal responses
  bitraverse resolveFailure (resolveSuccess responses) parsed
 where
  resolveFailure :: TCErr -> CommandM GiveFailure
  resolveFailure = lift . resolveGiveFailure path goal

  resolveSuccess :: [Response] -> String -> CommandM Edit
  resolveSuccess responses elaborated =
    lift $
      resolveGiveEdit goal (Text.pack expression) responses elaborated
        >>= either throwMismatch pure

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

resolveGiveFailure ::
  FilePath ->
  InteractionId ->
  TCErr ->
  TCM GiveFailure
resolveGiveFailure path goal e
  | IOException _ _ exception <- e =
      pure $ GiveIOFailed $ Text.pack $ displayException exception
  -- A give for a non-existent interaction ID fails `give_gen`'s first fallible
  -- operation, `lookupInteractionPoint` (MetaVars.hs:638-640), with this
  -- dedicated constructor. Match it before rendering so it isn't mistakenly
  -- reported as an error in the submitted expression.
  | TypeError _ _ closure <- e
  , InteractionError (NoSuchInteractionPoint _) <- clValue closure =
      pure UnknownGoal
  | otherwise = do
      path' <- liftIO $ absolute path
      interactionPoints <- useR stInteractionPoints
      let holeSpan = BiMap.lookup goal interactionPoints >>= fileSpan path' . ipRange
      GiveFailed holeSpan <$> resolveError path' e

resolveGiveEdit ::
  InteractionId ->
  Text ->
  [Response] ->
  String ->
  TCM (Either (AgdaResponseMismatch Response) Edit)
resolveGiveEdit goal submitted responses elaborated =
  maybe
    missing
    gave
    . Agda.Syntax.Position.rangeToInterval
    <$> getInteractionRange goal
 where
  gave interval =
    Right (Edit goal (toSpan interval) submitted (Text.pack elaborated))
  missing = Left (AgdaResponseMismatch "Cmd_give" responses)

-- TODO: Remove this check entirely?
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

exchange := given
          | givenThenIOFailed
          | failed

given := GiveAction (goal, Give_String s) -- give_gen:1021; noRange => Give_String (:1010)
         Status                           -- give_gen ends with `Cmd_metas` => display_info (:1024)
         DisplayInfo (Info_AllGoalsWarnings)  -- (:1145-1146); discarded, superseded by the reload
         InteractionPoints                -- runInteraction:268-271 (updateInteractionPointsAfter Cmd_give)

failed := DisplayInfo (Info_Error)       -- handleErr:216-242, as in matchLoad
          JumpToError?                   -- with noRange, typically absent
          HighlightingInfo
          Status                         -- hardcoded sChecked=False (:239)

givenThenIOFailed := GiveAction (goal, Give_String s)
                     failed(IOException)
                                      -- `Cmd_metas` calls `displayStatus`
                                      -- after emitting GiveAction. `status`
                                      -- reads the current file's mtime, so an
                                      -- environmental file-access failure can
                                      -- occur here. Agda rolls the command
                                      -- state back before emitting failed.

When the IOTCM's file is not `theCurrentFile`, `runInteraction` prefixes the
exchange with an implicit cmd_load' (Status ClearRunningInfo ClearHighlighting
RunningInfo*; InteractionTop.hs:257-263, cmd_load':848-869). `give` refuses
such calls with `GiveNotLoaded` before sending any Cmd_give, using the same
absolute-path comparison (`targetIsLoaded`), so a prelude-shaped response here
is a protocol violation.
-}
parseGiveResponses ::
  InteractionId ->
  [Response] ->
  Either (AgdaResponseMismatch Response) (Either TCErr String)
parseGiveResponses goal responses = maybe (Left violation) Right (exchange responses)
 where
  violation = AgdaResponseMismatch "Cmd_give" responses

  exchange rest = given rest <|> givenThenIOFailed rest <|> failed rest

  given
    ( Resp_GiveAction goal' (Give_String s)
        : Resp_Status _
        : Resp_DisplayInfo (Info_AllGoalsWarnings _ _)
        : [Resp_InteractionPoints _]
      )
      | goal' == goal = Just (Right s)
  given _ = Nothing

  givenThenIOFailed
    (Resp_GiveAction goal' (Give_String _) : rest)
      | goal' == goal
      , Just e@IOException {} <- failedTail id rest =
          Just (Left e)
  givenThenIOFailed _ = Nothing

  failed = failedTail Left

renderGiveResponse :: GiveResponse -> Text
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
renderGiveOutcome (GiveRejected (GiveRejection goal holeSpan e batch)) =
  "Give rejected for "
    <> renderGoalId goal
    <> " ("
    <> maybe "" (\s -> "at " <> renderSpan s <> "; ") holeSpan
    <> renderBatchPosition batch
    <> "). No file changes were made."
    <> "\n\n"
    <> renderRejectedError e
    <> "\n\nReloaded to resync:"
renderGiveOutcome (GiveUnknownGoal goal batch) =
  "No such goal "
    <> renderGoalId goal
    <> " ("
    <> renderBatchPosition batch
    <> "). No file changes were made. Goal IDs renumber after every edit or \
       \reload; use the IDs from the fresh list below.\n\nReloaded to resync:"
renderGiveOutcome GiveNotLoaded =
  "Give refused: the file is not the currently loaded file, and goal \
  \interaction IDs are only valid for the most recently loaded file. Nothing \
  \was checked and no changes were made. Loaded the file; use the goal IDs \
  \from the fresh result below:"
renderGiveOutcome GiveFileChanged =
  "Edits refused: the file on disk is not the version Agda loaded (it changed \
  \since the last load). No changes were made.\n\nReloaded to resync:"
renderGiveOutcome (GiveIOError e) =
  "The give could not be completed because the source file could not be accessed:\n\n"
    <> e
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
  header = renderGoalId $ editGoal edit
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

renderBatchPosition :: BatchPosition -> Text
renderBatchPosition (BatchPosition index batchLength) =
  "give "
    <> Text.pack (show (index + 1))
    <> " of "
    <> Text.pack (show batchLength)
