module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.Interact qualified as Interact
import Test.Interaction qualified as Interaction
import Test.MCP qualified as MCP
import Test.Model qualified as Model
import Test.Options qualified as Options
import Test.Tool qualified as Tool

main :: IO ()
main =
  defaultMain $
    testGroup
      "agda-mcp"
      [ Model.tests
      , Tool.tests
      , Interaction.tests
      , Options.tests
      , Interact.tests
      , MCP.tests
      ]
