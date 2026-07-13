-- Integration tests which drive `load` and `give` against the real Agda library
-- using `SessionM`. Fixtures are copied to a temporary directory per test (give
-- actually edits the file, etc).
module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Give qualified
import Load qualified

main :: IO ()
main = defaultMain $ testGroup "integration" [Load.tests, Give.tests]
