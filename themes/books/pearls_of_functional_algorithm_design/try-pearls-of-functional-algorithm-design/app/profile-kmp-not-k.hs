{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Ch17TheKnuthMorrisPrattAlgorithmFinal_1

main :: IO ()
main = print $ matches (5 `replicate` 'a') (100 `replicate` 'a')
