{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module QuickSort.Taocp where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Bool
import System.Random

largeM :: Int
largeM = 9

quicksort :: (Ord a, Bounded a) => [a] -> [a]
quicksort ks = init $ tail $ runST
	$ (>>) <$> qsort <*> getElems =<< newListArray' ks

newListArray' :: Bounded a => [a] -> ST s (STArray s Int a)
newListArray' xs = do
	a <- newArray_ (0, length xs + 1)
	writeArray a 0 minBound
	writeArray a (length xs + 1) maxBound
	a <$ uncurry (writeArray a) `mapM_` zip [1 ..] xs

qsort :: Ord a => STArray s Int a -> ST s ()
qsort ks = do
	(0, n1) <- getBounds ks
	let n = n1 - 1
	when (n > largeM) $ stage ks 1 n
	isort ks 1 n

stage :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
stage ks l r = do
	k <- readArray ks l
	(j, kj) <- exchange ks k (l + 1) r
	writeArray ks l kj
	writeArray ks j k
	case () of
		()	| r - j >= j - l && j - l > largeM ->
				stage ks l (j - 1) >> stage ks (j + 1) r
			| j - l > r - j && r - j > largeM ->
				stage ks (j + 1) r >> stage ks l (j - 1)
			| r - j > largeM && largeM >= j - l -> stage ks (j + 1) r
			| j - l > largeM && largeM >= r - j -> stage ks l (j - 1)
			| otherwise -> pure ()

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


mkSample :: (Int, Int) -> IO [Int]
mkSample r = do
	g <- newStdGen
	let	(n, g') = randomR (0, 200) g
	pure . take n $ randomRs r g'

mkSample' :: (Int, Int) -> Int -> IO [Int]
mkSample' r n = do
	g <- newStdGen
	pure . take n $ randomRs r g

checkSample :: Ord a => [a] -> Bool
checkSample = \case
	[] -> True
	[_] -> True
	x : xs@(y : _)
		| x <= y -> checkSample xs
		| otherwise -> False
