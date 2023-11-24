{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Timeout where

import Control.Concurrent
import System.Timeout

tryTimeout :: IO ()
tryTimeout = 5 `times` do
	maybe	(putStrLn "<no input>" >> threadDelay 1000000)
		putStrLn =<< timeout 1 getLine

tryTimeout2 :: IO ()
tryTimeout2 = 5 `times` do
	maybe	(putStrLn "<no input>" >> threadDelay 1000000)
		print =<< timeout 1 getChar

times :: Int -> IO () -> IO ()
n `times` act
	| n <= 0 = pure ()
	| otherwise = act >> (n - 1) `times` act
