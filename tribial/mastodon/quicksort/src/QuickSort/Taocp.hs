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
quicksortM m xs =
	init . tail $ runST $ (>>) <$> qsort m n <*> getElems =<< array n xs
	where n = length xs

array :: Bounded a => Int -> [a] -> ST s (STArray s Int a)
array n xs = newArray_ (0, n + 1) >>= \a -> do
	writeArray a 0 minBound; writeArray a (n + 1) maxBound
	a <$ uncurry (writeArray a) `mapM_` zip [1 ..] xs

qsort :: Ord a => Int -> Int -> STArray s Int a -> ST s ()
qsort m n xs = do
	when (n > m) $ stage m xs 1 n
	when (m > 1) $ isort n xs

stage :: Ord a => Int -> STArray s Int a -> Int -> Int -> ST s ()
stage m xs l r = readArray xs l >>= \k -> do
	(j, kj) <- exchange xs k (l + 1) r
	writeArray xs l kj; writeArray xs j k
	case (r - j >= j - l, r - j > m, j - l > m) of
		(True, _, True) -> stage m xs l (j - 1) >> stage m xs (j + 1) r
		(False, True, _) -> stage m xs (j + 1) r >> stage m xs l (j - 1)
		(_, True, False) -> stage m xs (j + 1) r
		(_, False, True) -> stage m xs l (j - 1)
		_ -> pure ()

exchange :: Ord a => STArray s Int a -> a -> Int -> Int -> ST s (Int, a)
exchange xs k i j = do 
	(i', ki) <- whileL i (< k) xs
	(j', kj) <- whileR j (k <) xs
	if j' <= i' then pure (j', kj) else do
		writeArray xs i' kj; writeArray xs j' ki
		exchange xs k (i' + 1) (j' - 1)

whileL :: Int -> (a -> Bool) -> STArray s Int a -> ST s (Int, a)
whileL i p a =
	readArray a i >>= \x -> bool (pure (i, x)) (whileL (i + 1) p a) (p x)

whileR :: Int -> (a -> Bool) -> STArray s Int a -> ST s (Int, a)
whileR j p a =
	readArray a j >>= \x -> bool (pure (j, x)) (whileR (j - 1) p a) (p x)
