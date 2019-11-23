{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Exts.Heap
import Control.Concurrent

import Lib

main :: IO ()
main = do
	test
	n <- read <$> getLine :: IO Int
	print =<< getClosureData n
	print n
	print =<< getClosureData n
	threadDelay 1000000
	print =<< getClosureData n
