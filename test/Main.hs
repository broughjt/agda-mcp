module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.Interaction.Load qualified as Load

main :: IO ()
main =
  defaultMain $
    testGroup
      "agda-mcp"
      [ testGroup
          "interaction"
          [ Load.tests
          ]
      ]
