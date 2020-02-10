{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseUniqueFunction where

import Control.Concurrent
import System.IO.Unsafe

import UniqueFunction

add123 :: Int -> Int
add123 n = unsafePerformIO $ (n + 123) <$ threadDelay 1000000

sample :: Count s (Int, Int)
sample = do
	f <- mkFun add123
	ff <- par f f
	apply ff 321

sample2 :: Count s (Int, Int)
sample2 = do
	f <- mkFun add123
	g <- mkFun add123
	fg <- par f g
	apply fg 321
