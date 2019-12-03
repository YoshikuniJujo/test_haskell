module Main where

import MorrisPrattAlgorithm

main :: IO ()
main = print $ matches (5 `replicate` 'a') (100 `replicate` 'a')
