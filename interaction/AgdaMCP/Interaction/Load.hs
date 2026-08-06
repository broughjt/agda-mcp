module AgdaMCP.Interaction.Load (
  load,
  Request (..),
  Response (..),
  LoadedFile (..),
  SourceHashUnavailable (..),
) where

import Agda.Interaction.Base (
  CommandState (..),
  CurrentFile (..),
  Rewrite (..),
 )
import Agda.Interaction.Command (CommandM)
import Agda.Interaction.Imports (Mode (..))
import Agda.Interaction.InteractionTop (cmd_load')
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.TypeChecking.Monad (TCErr (..))
import Agda.TypeChecking.Monad.Base (
  Interface (iSourceHash),
  ModuleInfo (miInterface),
 )
import Agda.TypeChecking.Monad.Imports (getVisitedModule)
import Agda.Utils.FileName (filePath)
import Agda.Utils.Hash (Hash)
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, lift, modify)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Extract (extractError)
import AgdaMCP.Interaction.Internal (InteractionM, catchTCErr, runCommandM)
import AgdaMCP.Interaction.Metas (extractMetas)
import AgdaMCP.Interaction.Model (
  Error,
  MetasReport,
 )
import Data.Text (Text)

-- To the best of my understanding, there are three paths the execution of a @Cmd_load@ can take: success, failure, and stale. Besides the interaction layer's general code described in the Main.hs comment, this execution amounts to the body of the @Cmd_load@ case of @interpret@, which consists of a call to @cmd_load'@ followed by a callback which runs @interpret Cmd_metas@. The first part of @cmd_load'@ emits a prelude of responses which is shared by all three paths:
--
--   * @Resp_Status@ (with sChecked false)
--   * @Resp_ClearRunningInfo@
--   * @Resp_ClearHighlighting NotOnlyTokenBased@
--   * Zero or more @Resp_RunningInfo@ unless we turn verbosity to @v0@
--
-- Then @cmd_load'@ parses the passed command-line arguments and calls @typeCheckMain@, both of which are fallible. @cmd_load'@ snapshots the file modification times before and after this block. If they match, then the following commands are emitted:
--
--   * @Resp_Status@
--   * @Resp_DisplayInfo (Info_AllGoalsWarnings)@
--   * @Resp_InteractionPoints@
--
-- These three are emitted as part of the interpretation of @Cmd_metas@ and in the @when@ block of @runInteraction@ for handling commands which modify the interaction points. The data in @Info_AllGoalsWarnings@ is the payload we're interested in. It contains a list of visible metavariables (goals the user can interact with), hidden metavariables (an unsolved @_@ for example), and a list of warnings and non-fatal errors. We don't collect this information from the @Resp_DisplayInfo@ response since we're intentionally ignoring emitted responses. Instead we query this information from Agda state, being careful to do it in the same manner as the relevant Agda code.
--
-- On the other hand, if the file modification times do not match, the current file and interaction point state is not updated. The responses for the success case are still emitted (except for @Resp_InteractionPoints@, which fails the file-is-still-current check in @runInteraction@), but they won't contain very much useful information. To detect this path we read back Agda's decision instead of repeating the check. See the comment in @load@ for details.
--
-- Finally, if an error occurs anywhere in the bodies of @cmd_load'@ or @interpret Cmd_metas@, it is thrown as a @TCErr@ exception and caught in @handleCommand@. See the discussion in app/Main.hs about this function, but the TL;DR is that I'm pretty confident that it suffices to just @catchTCErr@ locally in our wrapper for load. The one thing we have to look after is the branch in @runInteraction@'s @onFail@ (InteractionTop.hs:285-286), which makes sure to set @theCurrentFile@ to @Nothing@ on top of the state resets performed by the @MonadError@ implementations for @CommandM@ and @TCM@.

-- @Cmd_load@ takes a path to load and a list of command-line arguments to apply.
data Request = Request {requestPath :: FilePath, requestArguments :: [Text]}
  deriving (Eq, Show)

data Response
  = ResponseOk LoadedFile MetasReport
  | ResponseError Error
  | ResponseStale
  deriving (Eq, Show)

data LoadedFile = LoadedFile
  { loadedFilePath :: FilePath
  , loadedFileSourceHash :: Hash
  }
  deriving (Eq, Show)

load :: Request -> InteractionM Response
load = runCommandM . loadInternal

loadInternal :: Request -> CommandM Response
loadInternal (Request path arguments) = do
  -- The catch covers only @cmd_load'@ and the staleness read, which are the
  -- only steps where a @TCErr@ is a genuine load outcome. Errors during
  -- extraction should be treated as bugs in our code.
  outcome <-
    ( do
        cmd_load' path (map Text.unpack arguments) True TypeCheck $ const $ pure ()
        -- @cmd_load'@ clears @theCurrentFile@ as its first action and resets it
        -- only inside the @when (t == t')@ block, so after a non-throwing return
        -- @theCurrentFile@ is @Just@ exactly when the fresh path was
        -- taken. Reading it back therefore distinguishes the two with no race
        -- against further disk changes.
        Right <$> gets theCurrentFile
    )
      `catchTCErr` handler
  case outcome of
    Right (Just current) ->
      ResponseOk <$> extractLoadedFile current <*> lift (extractMetas AsIs)
    Right Nothing -> pure ResponseStale
    Left e -> pure $ ResponseError e
 where
  handler :: TCErr -> CommandM (Either Error (Maybe CurrentFile))
  handler e = do
    -- Even though @cmd_load'@ clears @theCurrentFile@ during its setup
    -- (:851-853), the automatic state rollback in the @StateT@ and @TCM@
    -- @MonadError@ instances would restore the previously loaded file when the
    -- load throws. The @onFail@ clause in @runInteraction@ sets it back to
    -- @Nothing@. We match this behavior here.
    modify $ \state -> state {theCurrentFile = Nothing}
    Left <$> lift (extractError e)

extractLoadedFile :: CurrentFile -> CommandM LoadedFile
extractLoadedFile current =
  -- @visitModule@ records the interface even when the module has warnings such
  -- as open holes (Imports.hs:467; warnings only skip @storeDecodedModule@), so
  -- the module we have just loaded always has one. If it does not, our mental
  -- model of Agda is wrong.
  lift (getVisitedModule $ currentFileModule current)
    >>= maybe unavailable loaded
 where
  path :: FilePath
  path = filePath $ currentFilePath current

  unavailable :: CommandM LoadedFile
  unavailable =
    liftIO $
      throwIO $
        SourceHashUnavailable path $
          Text.pack $
            prettyShow (currentFileModule current)

  loaded :: ModuleInfo -> CommandM LoadedFile
  loaded = pure . LoadedFile path . iSourceHash . miInterface

-- Bug: the module Agda reported as loaded has no visited interface, so the
-- source hash callers fingerprint edits against cannot be obtained.
data SourceHashUnavailable = SourceHashUnavailable FilePath Text
  deriving (Show)

instance Exception SourceHashUnavailable
