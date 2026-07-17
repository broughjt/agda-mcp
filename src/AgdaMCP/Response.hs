module AgdaMCP.Response (
  AgdaResponseMismatch (..),
  throwMismatch,
  runParseResponses,
  responseToken,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)

import Agda.Interaction.JSON (EncodeTCM (encodeTCM))
import Agda.Interaction.JSONTop ()
import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  GoalDisplayInfo_boot (..),
  Info_Error_boot (..),
  Response,
  Response_boot (..),
 )

import Agda.TypeChecking.Monad (TCM)
import Data.Bifunctor (first)
import Text.Parsec (
  ParseError,
  Parsec,
  incSourceColumn,
  runParser,
  tokenPrim,
 )

-- Response parsing

type Parser = Parsec [Response] ()

runParseResponses ::
  String -> Parser a -> [Response] -> Either (AgdaResponseMismatch Response) a
runParseResponses command parser responses =
  first (AgdaResponseMismatch command responses . Just) $
    runParser parser () "" responses

responseToken :: (Response -> Maybe a) -> Parser a
responseToken testToken =
  tokenPrim
    showResponse
    nextPosition
    testToken
 where
  -- Convention: we treat each index into the response list as a source
  -- column. Note that lines and columns are one-based in Parsec.
  nextPosition position _ _ = incSourceColumn position 1

-- TODO: Not having `Show` is a serious pain. Work with a newtype over `Response` that uses `showResponse`.

showResponse :: Response -> String
showResponse (Resp_HighlightingInfo _ _ _ _) = "Resp_HighlightingInfo ..."
showResponse (Resp_Status _) = "Resp_Status ..."
showResponse (Resp_JumpToError _ _) = "Resp_JumpToError ..."
showResponse (Resp_InteractionPoints _) = "Resp_InteractionPoints ..."
showResponse (Resp_GiveAction _ _) = "Resp_GiveAction ..."
showResponse (Resp_MakeCase _ _ _) = "Resp_MakeCase ..."
showResponse (Resp_SolveAll _) = "Resp_SolveAll ..."
showResponse (Resp_Mimer _ _) = "Resp_Mimer ..."
showResponse (Resp_DisplayInfo (Info_CompilationOk _ _)) = "Resp_DisplayInfo (Info_CompilationOk ...)"
showResponse (Resp_DisplayInfo (Info_Constraints _)) = "Resp_DisplayInfo (Info_Constraints ...)"
showResponse (Resp_DisplayInfo (Info_AllGoalsWarnings _ _)) = "Resp_DisplayInfo (Info_AllGoalsWarnings ...)"
showResponse (Resp_DisplayInfo (Info_Time _)) = "Resp_DisplayInfo (Info_Time ...)"
showResponse (Resp_DisplayInfo (Info_Error (Info_GenericError _))) = "Resp_DisplayInfo (Info_Error (Info_GenericError ...))"
showResponse (Resp_DisplayInfo (Info_Error (Info_CompilationError _))) = "Resp_DisplayInfo (Info_Error (Info_CompilationError ...))"
showResponse (Resp_DisplayInfo (Info_Error (Info_HighlightingParseError _))) = "Resp_DisplayInfo (Info_Error (Info_HighlightingParseError ...))"
showResponse (Resp_DisplayInfo (Info_Error (Info_HighlightingScopeCheckError _))) = "Resp_DisplayInfo (Info_Error (Info_HighlightingScopeCheckError ...))"
showResponse (Resp_DisplayInfo Info_Intro_NotFound) = "Resp_DisplayInfo Info_Intro_NotFound"
showResponse (Resp_DisplayInfo (Info_Intro_ConstructorUnknown _)) = "Resp_DisplayInfo (Info_Intro_ConstructorUnknown ...)"
showResponse (Resp_DisplayInfo (Info_Auto _)) = "Resp_DisplayInfo (Info_Auto ...)"
showResponse (Resp_DisplayInfo (Info_ModuleContents _ _ _)) = "Resp_DisplayInfo (Info_ModuleContents ...)"
showResponse (Resp_DisplayInfo (Info_SearchAbout _ _)) = "Resp_DisplayInfo (Info_SearchAbout ...)"
showResponse (Resp_DisplayInfo (Info_WhyInScope _)) = "Resp_DisplayInfo (Info_WhyInScope ...)"
showResponse (Resp_DisplayInfo (Info_NormalForm _ _ _ _)) = "Resp_DisplayInfo (Info_NormalForm ...)"
showResponse (Resp_DisplayInfo (Info_InferredType _ _ _)) = "Resp_DisplayInfo (Info_InferredType ...)"
showResponse (Resp_DisplayInfo (Info_Context _ _)) = "Resp_DisplayInfo (Info_Context ...)"
showResponse (Resp_DisplayInfo Info_Version) = "Resp_DisplayInfo Info_Version"
showResponse (Resp_DisplayInfo (Info_GoalSpecific _ (Goal_HelperFunction _))) = "Resp_DisplayInfo (Info_GoalSpecific ... (Goal_HelperFunction ...))"
showResponse (Resp_DisplayInfo (Info_GoalSpecific _ (Goal_NormalForm _ _))) = "Resp_DisplayInfo (Info_GoalSpecific ... (Goal_NormalForm ...))"
showResponse (Resp_DisplayInfo (Info_GoalSpecific _ (Goal_GoalType _ _ _ _ _))) = "Resp_DisplayInfo (Info_GoalSpecific ... (Goal_GoalType ...))"
showResponse (Resp_DisplayInfo (Info_GoalSpecific _ (Goal_CurrentGoal _))) = "Resp_DisplayInfo (Info_GoalSpecific ... (Goal_CurrentGoal ...))"
showResponse (Resp_DisplayInfo (Info_GoalSpecific _ (Goal_InferredType _))) = "Resp_DisplayInfo (Info_GoalSpecific ... (Goal_InferredType ...))"
showResponse (Resp_RunningInfo _ _) = "Resp_RunningInfo ..."
showResponse Resp_ClearRunningInfo = "Resp_ClearRunningInfo"
showResponse (Resp_ClearHighlighting _) = "Resp_ClearHighlighting ..."
showResponse Resp_DoneAborting = "Resp_DoneAborting"
showResponse Resp_DoneExiting = "Resp_DoneExiting"

-- Expected vs actual response list mismatches

-- A list of responses emitted by Agda did not match our mental model for the
-- pattern of possible responses.
data AgdaResponseMismatch a
  = AgdaResponseMismatch
  { mismatchCommand :: String
  , mismatchResponses :: [a]
  , mismatchParseError :: Maybe ParseError
  }
  deriving (Foldable, Functor, Show, Traversable)

instance Exception (AgdaResponseMismatch Value)

--- An `AgdaResponseMismatch` is always a bug in agda-mcp. If we encounter one,
--- we encode the raw responses to JSON while their type-checking state is
--- available in the type-checking monad and then die loudly with the resulting
--- debugging information.
throwMismatch :: AgdaResponseMismatch Response -> TCM a
throwMismatch mismatch = traverse encodeTCM mismatch >>= liftIO . throwIO
