{-# LANGUAGE LambdaCase #-}

module AgdaMCP.Commands (
  LoadOutcome (..),
  LoadResult (..),
  load,
  matchLoad,
) where

import Agda.Interaction.Base (IOTCM' (..), Interaction' (..))
import Agda.Interaction.EmacsTop (showGoals, showInfoError)
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  Goals,
  Info_Error,
  Response,
  Response_boot (..),
 )
import Agda.Syntax.Common (InteractionId)
import Agda.TypeChecking.Monad (
  HighlightingLevel (..),
  HighlightingMethod (..),
  TCM,
 )
import Agda.TypeChecking.Monad.Base (WarningsAndNonFatalErrors (..))
import Agda.TypeChecking.Pretty.Warning (prettyTCWarnings)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Worker (Command (..), ProtocolViolation (..))

data LoadResult
  = Loaded Text [InteractionId]
  | LoadFailed Text
  | LoadStale
  deriving (Show)

load :: FilePath -> Command LoadResult
load path =
  Command
    { commandIOTCM = IOTCM path None Direct (Cmd_load path [])
    , commandParse = traverse renderLoadOutcome . matchLoad
    }

-- The well-formed shapes of a Cmd_load exchange, before rendering.
data LoadOutcome
  = LoadGoals Goals WarningsAndNonFatalErrors [InteractionId]
  | LoadError Info_Error
  | LoadNotRegistered

{- The grammar of a Cmd_load exchange, per the Agda v2.8.0 source (file is
src/full/Agda/Interaction/InteractionTop.hs unless said otherwise; matchLoad
below mirrors it clause for clause):

  exchange := prelude checking
            | failed                    -- error before cmd_load' started:
                                        -- runInteraction fails already at
                                        -- absolute current (:255)
  prelude  := Status                    -- cmd_load' opens with displayStatus
                                        -- (:857); nothing before it can fail
              ClearRunningInfo          -- :866
              ClearHighlighting         -- :869
  checking := RunningInfo checking      -- progress messages while type
                                        -- checking, routed through the
                                        -- callback by the debug machinery
                                        -- (TypeChecking/Monad/Debug.hs:156);
                                        -- count varies with imports and
                                        -- verbosity
            | loaded
            | failed                    -- the first failable operation is
                                        -- parsing the path (:874), so a
                                        -- TCErr can strike only after the
                                        -- full prelude
  loaded   := Status                    -- Cmd_metas (:508-510) reports via
              DisplayInfo               -- display_info = displayStatus +
                (Info_AllGoalsWarnings)   payload (:1143-1146); the only
                                        -- display_info on the success path
              InteractionPoints?        -- sent last by runInteraction
                                        -- (:267-271) because
                                        -- updateInteractionPointsAfter
                                        -- Cmd_load = True (:425) — unless
                                        -- the file's modification time
                                        -- changed during checking, in which
                                        -- case cmd_load' discarded the
                                        -- points (:907-916) and the guard
                                        -- (:268-270) suppresses them:
                                        -- checked but stale
  failed   := DisplayInfo (Info_Error)  -- every TCErr (and IOException /
                                        -- SomeException, converted by
                                        -- handleNastyErrors, :194-208)
                                        -- lands in handleErr (:216-242),
                                        -- which emits these four in one
                                        -- mapM_ putResponse (:234-242):
              JumpToError?              -- only when the error's range
                                        -- starts at a file position
                                        -- (:1185-1191)
              HighlightingInfo          -- emitted even at HighlightingLevel
                                        -- None; handleErr computes the
                                        -- error highlighting itself
              Status

handleErr emits nothing at all when the rendered error is empty (:219-220);
the exchange is then a bare prelude, which matchLoad rejects — a silently
failed load is exactly the kind of surprise that should explode. -}
matchLoad :: [Response] -> Either (ProtocolViolation Response) LoadOutcome
matchLoad responses = maybe (Left violation) Right (exchange responses)
 where
  violation = ProtocolViolation "Cmd_load" responses

  exchange (Resp_Status {} : Resp_ClearRunningInfo : Resp_ClearHighlighting {} : rest) =
    checking rest
  exchange rest = failed rest

  checking (Resp_RunningInfo {} : rest) = checking rest
  checking (Resp_Status {} : Resp_DisplayInfo (Info_AllGoalsWarnings goals warnings) : rest) =
    loaded goals warnings rest
  checking rest = failed rest

  loaded goals warnings [Resp_InteractionPoints ids] = Just (LoadGoals goals warnings ids)
  loaded _ _ [] = Just LoadNotRegistered
  loaded _ _ _ = Nothing

  failed (Resp_DisplayInfo (Info_Error err) : rest) = case rest of
    [Resp_JumpToError {}, Resp_HighlightingInfo {}, Resp_Status {}] -> Just (LoadError err)
    [Resp_HighlightingInfo {}, Resp_Status {}] -> Just (LoadError err)
    _ -> Nothing
  failed _ = Nothing

renderLoadOutcome :: LoadOutcome -> TCM LoadResult
renderLoadOutcome = \case
  LoadGoals goals warnings ids -> do
    goalsText <- showGoals goals
    errorsText <- prettyTCWarnings (nonFatalErrors warnings)
    warningsText <- prettyTCWarnings (tcWarnings warnings)
    let body = intercalate "\n" (filter (not . null) [goalsText, errorsText, warningsText])
    pure (Loaded (Text.pack body) ids)
  LoadError err -> LoadFailed . Text.pack <$> showInfoError err
  LoadNotRegistered -> pure LoadStale
