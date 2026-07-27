module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.Interaction qualified as Interaction

main :: IO ()
main =
  defaultMain $ testGroup "agda-mcp" [Interaction.tests]
