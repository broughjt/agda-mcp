module AgdaMCP.Tools.Internal (ToolState, newToolState) where

import AgdaMCP.Interaction (InteractionState, newInteractionState)

data ToolState = ToolState {toolInteractionState :: InteractionState}

newToolState :: IO ToolState
newToolState = ToolState <$> newInteractionState
