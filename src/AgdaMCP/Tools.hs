module AgdaMCP.Tools (
  caseSplitTool,
  checkTool,
  giveTool,
  goalTool,
  loadTool,
  ToolState,
  newToolState,
) where

import AgdaMCP.Tools.CaseSplit (caseSplitTool)
import AgdaMCP.Tools.Check (checkTool)
import AgdaMCP.Tools.Give (giveTool)
import AgdaMCP.Tools.Goal (goalTool)
import AgdaMCP.Tools.Load (loadTool)
import AgdaMCP.Tools.State (ToolState, newToolState)
