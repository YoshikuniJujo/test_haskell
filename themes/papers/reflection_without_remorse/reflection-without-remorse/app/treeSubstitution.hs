{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import TreeSubstitution

main :: IO ()
main = do
	print $ {-# SCC "LEFT" #-} (sample1 <-- sample2) <-- sample3
	print $ {-# SCC "RIGHT" #-} sample1 <-- (sample2 <-- sample3)
