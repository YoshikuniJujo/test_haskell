{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent.STM
import Control.Exception
import System.IO

import Human.Event

main :: IO ()
main = do
	bm <- hGetBuffering stdin
	print bm
	finally (hSetBuffering stdin bm) do
		hSetBuffering stdin NoBuffering
		ch <- hGetAndPushCChar stdin
		forever $ print =<< atomically (readTChan ch)
