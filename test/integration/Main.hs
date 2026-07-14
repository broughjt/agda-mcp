-- Integration tests which drive `load`, `give`, and `goal` against the real
-- Agda library using `SessionM`. Fixtures are copied to a temporary directory
-- per test (give actually edits the file, etc).
module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Give qualified
import Goal qualified
import Load qualified

main :: IO ()
main =
  defaultMain $ testGroup "integration" [Load.tests, Give.tests, Goal.tests]
