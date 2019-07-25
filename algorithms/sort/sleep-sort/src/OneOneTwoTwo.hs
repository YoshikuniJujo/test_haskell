{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OneOneTwoTwo where

import Control.Concurrent
import System.IO.Unsafe
import System.Random

import NoOrderList

f :: Int -> [Int]
f = unsafePerformIO . repeatIO

repeatIO :: Int -> IO [Int]
repeatIO n = unsafeInterleaveIO $ do
	threadDelay =<< randomRIO (100000, 3000000)
	(n :) <$> repeatIO n

mergeList :: [a] -> [a] -> IO [a]
mergeList xs ys = do
	c <- newChan
	_ <- forkIO $ mapM_ (writeChan c) xs
	_ <- forkIO $ mapM_ (writeChan c) ys
	readChans c

mergeListNoOrder :: [a] -> [a] -> NoOrderList a
mergeListNoOrder xs ys = fromList . unsafePerformIO $ mergeList xs ys

readChans :: Chan a -> IO [a]
readChans c = unsafeInterleaveIO $ do
	x <- readChan c
	(x :) <$> readChans c

try :: IO [Int]
try = mergeList (f 1) (f 2)
