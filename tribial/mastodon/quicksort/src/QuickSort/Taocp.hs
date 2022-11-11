{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module QuickSort.Taocp where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Bool
import System.Random

quicksort :: (Ord a, Bounded a) => [a] -> [a]
quicksort ks = init $ tail $ runST
	$ (>>) <$> qsort mixM <*> getElems =<< newListArray' ks

quicksortM :: (Ord a, Bounded a) => Int -> [a] -> [a]
quicksortM m ks = init $ tail $ runST
	$ (>>) <$> qsort m <*> getElems =<< newListArray' ks

newListArray' :: Bounded a => [a] -> ST s (STArray s Int a)
newListArray' xs = do
	a <- newArray_ (0, length xs + 1)
	writeArray a 0 minBound
	writeArray a (length xs + 1) maxBound
	a <$ uncurry (writeArray a) `mapM_` zip [1 ..] xs

mixM :: Int
mixM = 9

qsort :: Ord a => Int -> STArray s Int a -> ST s ()
qsort m ks = do
	(_, n1) <- getBounds ks
	let n = n1 - 1
	when (n > m) $ stage m ks 1 n
	when (m > 1) $ isort ks 1 n

stage :: Ord a => Int -> STArray s Int a -> Int -> Int -> ST s ()
stage m ks l r = do
	k <- readArray ks l
	(j, kj) <- exchange ks k (l + 1) r
	writeArray ks l kj
	writeArray ks j k
	case (r - j >= j - l, r - j > m, j - l > m) of
		(True, _, True) -> stage m ks l (j - 1) >> stage m ks (j + 1) r
		(False, True, _) -> stage m ks (j + 1) r >> stage m ks l (j - 1)
		(_, True, False) -> stage m ks (j + 1) r
		(_, False, True) -> stage m ks l (j - 1)
		_ -> pure ()

exchange :: Ord a => STArray s Int a -> a -> Int -> Int -> ST s (Int, a)
exchange ks k i j = do 
	(i', ki) <- whileLeft i (< k) ks
	(j', kj) <- whileRight j (k <) ks
	if j' <= i' then pure (j', kj) else do
		writeArray ks i' kj
		writeArray ks j' ki
		exchange ks k (i' + 1) (j' - 1)

whileLeft :: Int -> (a -> Bool) -> STArray s Int a -> ST s (Int, a)
whileLeft i p a = do
	x <- readArray a i
	bool (pure (i, x)) (whileLeft (i + 1) p a) (p x)

whileRight :: Int -> (a -> Bool) -> STArray s Int a -> ST s (Int, a)
whileRight j p a  = do
	x <- readArray a j
	bool (pure (j, x)) (whileRight (j - 1) p a) (p x)

isort :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
isort ks l r = for_ [l + 1 .. r] \j -> do
	k <- readArray ks j
	isort1 ks (j - 1) k

isort1 :: Ord a => STArray s Int a -> Int -> a -> ST s ()
isort1 ks i k = do
	ki <- readArray ks i
	if ki <= k then writeArray ks (i + 1) k else do
		writeArray ks (i + 1) =<< readArray ks i
		isort1 ks (i - 1) k
