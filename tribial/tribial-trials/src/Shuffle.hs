{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shuffle where

import Data.List
import System.Random

format :: String -> [Char] -> String
format "" _ = ""
format ('_' : s) (c : cs) = c : format s cs
format (c : s) cs = c : format s cs

shuffle :: [a] -> IO [a]
shuffle xs = do
	i <- randomRIO (0, product [1 .. length xs])
	pure $ permutations xs !! i
