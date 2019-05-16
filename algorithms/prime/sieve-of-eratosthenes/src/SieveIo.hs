{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SieveIo (sieveIo) where

import Control.Arrow (first)
import Data.Foldable (for_)
import Data.Array.IO (IOArray, newArray, getBounds, readArray, writeArray)
import Data.Bool (bool)

sieveIo :: Int -> IO [Int]
sieveIo n | n < 2 = error "sieveIo n: n should be larger than 1"
sieveIo n = do
	a <- newArray (2, n) True
	(ps, l) <- sieve 2 a
	(ps ++) <$> nexts l a
	where
	sieve i _ | i ^ (2 :: Int) > n = return ([i], i)
	sieve i a = do
		remove i a
		Just i' <- next i a
		first (i :) <$> sieve i' a

next :: Int -> IOArray Int Bool -> IO (Maybe Int)
next n a = do
	(_, mx) <- getBounds a
	if n' > mx
		then return Nothing
		else bool (next n' a) (return $ Just n') =<< readArray a n'
	where n' = n + 1

remove :: Int -> IOArray Int Bool -> IO ()
remove n a =  do
	(_, mx) <- getBounds a
	for_ (takeWhile (<= mx) [2 * n, 3 * n ..]) $ flip (writeArray a) False

nexts :: Int -> IOArray Int Bool -> IO [Int]
nexts n a = maybe (return []) (\n' -> (n' :) <$> nexts n' a) =<< next n a
