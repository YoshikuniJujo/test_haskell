{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OneOneTwoTwo where

import Control.Monad
import Control.Concurrent
import System.IO.Unsafe
import System.Random

f :: Int -> [Int]
f = unsafePerformIO . repeatIO

repeatIO :: Int -> IO [Int]
repeatIO n = unsafeInterleaveIO $ do
	threadDelay =<< randomRIO (100000, 3000000)
	(n :) <$> repeatIO n

mergeList :: [a] -> [a] -> IO [a]
mergeList xs ys = do
	c <- newChan
	forkIO $ mapM_ (writeChan c) xs
	forkIO $ mapM_ (writeChan c) ys
	readChans c

readChans :: Chan a -> IO [a]
readChans c = unsafeInterleaveIO $ do
	x <- readChan c
	(x :) <$> readChans c
