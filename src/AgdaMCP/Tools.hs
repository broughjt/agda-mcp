module AgdaMCP.Tools (
  caseSplitTool,
  checkTool,
  giveTool,
  goalTool,
  inferTool,
  loadTool,
  ToolState,
  newToolState,
) where

import AgdaMCP.Tools.CaseSplit (caseSplitTool)
import AgdaMCP.Tools.Check (checkTool)
import AgdaMCP.Tools.Give (giveTool)
import AgdaMCP.Tools.Goal (goalTool)
import AgdaMCP.Tools.Infer (inferTool)
import AgdaMCP.Tools.Load (loadTool)
import AgdaMCP.Tools.State (ToolState, newToolState)
