{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Trial.Count

main :: IO ()
main = do
	print =<< tryLeftCount
	print =<< tryLeftCountSig
	print =<< tryLeftRandomSig
