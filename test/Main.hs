module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import AgdaMCP.PositionTest qualified as PositionTest
import AgdaMCP.Tools.GiveTest qualified as GiveTest
import AgdaMCP.Tools.LoadTest qualified as LoadTest

main :: IO ()
main =
  defaultMain $
    testGroup
      "rendering"
      [ PositionTest.tests
      , LoadTest.tests
      , GiveTest.tests
      ]
