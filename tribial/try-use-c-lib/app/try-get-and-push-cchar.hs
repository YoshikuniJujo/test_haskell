{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Bool
import Data.Char
import System.IO

import Human.Event

main :: IO ()
main = hGetBuffering stdin >>= \bm -> finally (hSetBuffering stdin bm) do
	hSetBuffering stdin NoBuffering
	ch <- hGetAndPushCChar stdin
	forever $ do
		c <- atomically
			$ bool (readTChan ch) (pure . fromIntegral $ ord '\0')
				=<< isEmptyTChan ch
		bool (print c) (threadDelay 1000000 >> putStrLn "TICK") (c == 0)
