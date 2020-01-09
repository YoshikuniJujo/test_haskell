{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Eff
import State

main :: IO ()
main = do
	print . run $ sampleL `runState` (0 :: Integer)
	print . run $ sampleR `runState` (0 :: Integer)
