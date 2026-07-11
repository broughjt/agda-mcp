module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import AgdaMCP.Tools.GiveTest qualified as GiveTest
import AgdaMCP.Tools.LoadTest qualified as LoadTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "rendering"
      [ LoadTest.tests
      , GiveTest.tests
      ]
