module Main (main) where

import AgdaMCP (foo)

main :: IO ()
main = putStrLn $ show $ foo 5
