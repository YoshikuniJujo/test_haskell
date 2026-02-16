module Main (main) where

import UUIDv7Gen

main :: IO ()
main = print =<< nextUUIDv7
