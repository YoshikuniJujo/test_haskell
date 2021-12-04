{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Char
import System.IO

import Human.Event

main :: IO ()
main = do
	bm <- hGetBuffering stdin
	print bm
	finally (hSetBuffering stdin bm) do
		hSetBuffering stdin NoBuffering
		ch <- hGetAndPushCChar stdin
		forever $ do
			c <- atomically do
				b <- isEmptyTChan ch
				if b
				then pure (fromIntegral $ ord '\0')
				else readTChan ch
			if c == 0
			then threadDelay 1000000 >> putStrLn "TICK"
			else print c
