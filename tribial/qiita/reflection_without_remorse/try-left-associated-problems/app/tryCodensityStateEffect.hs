{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Eff
import State
import CodensityStateEffect

main :: IO ()
main = do
	print $ {-# SCC "RunLeftAssociatedCounter" #-}
		run $ sampleLEffect `runState` (0 :: Integer)
	print $ {-# SCC "RunRightAssociatedCounter" #-}
		run $ sampleREffect `runState` (0 :: Integer)
