{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module QuickSort.Taocp (quicksort, quicksortM) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Bool

import StraightInsertionSort

quicksort :: (Ord a, Bounded a) => [a] -> [a]
quicksort = quicksortM 32

quicksortM :: (Ord a, Bounded a) => Int -> [a] -> [a]
quicksortM m ks =
	init . tail $ runST $ (>>) <$> qsort m n <*> getElems =<< array n ks
	where n = length ks

array :: Bounded a => Int -> [a] -> ST s (STArray s Int a)
array n ks = newArray_ (0, n + 1) >>= \a -> do
	writeArray a 0 minBound; writeArray a (n + 1) maxBound
	a <$ uncurry (writeArray a) `mapM_` zip [1 ..] ks

qsort :: Ord a => Int -> Int -> STArray s Int a -> ST s ()
qsort m n ks = do
	when (n > m) $ stage m ks 1 n
	when (m > 1) $ isort n ks

stage :: Ord a => Int -> STArray s Int a -> Int -> Int -> ST s ()
stage m ks l r = readArray ks l >>= \k -> do
	(j, kj) <- exchange ks k (l + 1) r
	writeArray ks l kj; writeArray ks j k
	case (r - j >= j - l, r - j > m, j - l > m) of
		(True, _, True) -> stage m ks l (j - 1) >> stage m ks (j + 1) r
		(False, True, _) -> stage m ks (j + 1) r >> stage m ks l (j - 1)
		(_, True, False) -> stage m ks (j + 1) r
		(_, False, True) -> stage m ks l (j - 1)
		_ -> pure ()

exchange :: Ord a => STArray s Int a -> a -> Int -> Int -> ST s (Int, a)
exchange ks k i j = do
	(i', ki) <- fromL i (< k) ks
	(j', kj) <- fromR j (k <) ks
	if j' <= i' then pure (j', kj) else do
		writeArray ks i' kj; writeArray ks j' ki
		exchange ks k (i' + 1) (j' - 1)

fromL :: Int -> (a -> Bool) -> STArray s Int a -> ST s (Int, a)
fromL i p ks =
	readArray ks i >>= \k -> bool (pure (i, k)) (fromL (i + 1) p ks) (p k)

fromR :: Int -> (a -> Bool) -> STArray s Int a -> ST s (Int, a)
fromR j p ks =
	readArray ks j >>= \k -> bool (pure (j, k)) (fromR (j - 1) p ks) (p k)
