module AgdaMCP.Tools.Common (
  failedTail,
  runCommand,
) where

import Control.Exception (throwIO)

import Agda.Interaction.Response (
  DisplayInfo_boot (..),
  Info_Error,
  RemoveTokenBasedHighlighting (..),
  Response,
  Response_boot (..),
  Status (..),
 )

import AgdaMCP.Worker (
  Command,
  Worker,
  sendCommand,
 )

-- A `Failure` is a bug in agda-mcp, not a runtime exception we should
-- recover. We throw it here at the tool-handler boundary and deliberately catch
-- it nowhere. This causes the process to die and the dump the error to stderr.
runCommand :: Worker -> Command r -> IO r
runCommand worker command =
  sendCommand worker command >>= either throwIO pure

failedTail :: (Info_Error -> a) -> [Response] -> Maybe a
failedTail wrap (Resp_DisplayInfo (Info_Error e) : rest) = case rest of
  [ Resp_JumpToError {}
    , Resp_HighlightingInfo _ KeepHighlighting _ _
    , Resp_Status status
    ]
      | not (sChecked status) -> Just (wrap e)
  [Resp_HighlightingInfo _ KeepHighlighting _ _, Resp_Status status]
    | not (sChecked status) -> Just (wrap e)
  _ -> Nothing
failedTail _ _ = Nothing
