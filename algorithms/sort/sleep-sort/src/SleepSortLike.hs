{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SleepSortLike where

import Control.Monad
import Data.Foldable
import Control.Concurrent

import System.IO.Unsafe

sleepSortLike :: (a -> b) -> [a] -> IO [b]
sleepSortLike f xs = do
	c <- newChan
	for_ xs $ \x -> forkIO $ writeChan c $! f x -- let fx = f x  in fx `seq` writeChan c fx
	replicateM (length xs) $ unsafeInterleaveIO $ readChan c

fib :: Int -> Integer
fib n | n < 1 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

tryIt :: IO [Integer]
tryIt = sleepSortLike fib [34, 15, 1, 3, 10]
