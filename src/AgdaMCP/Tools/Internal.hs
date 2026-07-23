module AgdaMCP.Tools.Internal (ToolState, newToolState) where

import Agda.Interaction.Options (defaultOptions)

import AgdaMCP.Interaction (InteractionState, newInteractionState)

data ToolState = ToolState {toolInteractionState :: InteractionState}

-- TODO: Take Agda command-line configuration
newToolState :: IO ToolState
newToolState = ToolState <$> newInteractionState defaultOptions
