{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Concurrent
import Control.Concurrent.STM

main :: IO ()
main = do
	p1 <- async do
		putStrLn "HELLO"
		threadDelay 1000000
		putStrLn "WORLD"
		pure 123
	p2 <- async do
		putStrLn "GOOD"
		threadDelay 500000
		putStrLn "BYE"
		pure 456
	print =<< await p1
	print =<< await p2

type Promise a = TChan a

async :: IO a -> IO (Promise a)
async act = do
	c <- atomically newTChan
	forkIO $ atomically . writeTChan c =<< act
	pure c

await :: Promise a -> IO a
await = atomically . readTChan
