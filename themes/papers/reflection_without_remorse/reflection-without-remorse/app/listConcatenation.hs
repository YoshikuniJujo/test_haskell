{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding ((++))

import ListConcatenation

main :: IO ()
main = do
	putStrLn $ {-# SCC "LEFT" #-} ("hello" ++ "beautiful") ++ "world"
	putStrLn $ {-# SCC "RIGHT" #-} "hello" ++ ("beautiful" ++ "world")
