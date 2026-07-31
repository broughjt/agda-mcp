module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.Interaction qualified as Interaction
import Test.Model qualified as Model
import Test.Tool qualified as Tool

main :: IO ()
main =
  defaultMain $ testGroup "agda-mcp" [Model.tests, Tool.tests, Interaction.tests]
