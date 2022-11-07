{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module QuickSort.Simple (quicksort) where

import Control.Monad.ST
import Data.Array.ST
import Data.Bool

quicksort :: Ord a => [a] -> [a]
quicksort xs = runST
	$ (>>) <$> qsort 0 e <*> getElems =<< newListArray (0, e) xs
	where e = length xs - 1

qsort :: Ord a => Int -> Int -> STArray s Int a -> ST s ()
qsort i j _ | i >= j = pure ()
qsort i j _ | i + 1 >= j = pure ()
qsort i j _ | i + 2 >= j = pure ()
qsort i j _ | i + 3 >= j = pure ()
qsort i j a = do
	p <- pivot i j a
	(i', j') <- flipAll p i j a
	qsort i j' a
	qsort i' j a

flipAll :: Ord a => a -> Int -> Int -> STArray s Int a -> ST s (Int, Int)
flipAll p i j a = do
	Just (i', x) <- findLeft (>= p) i j a
	findRight (< p) i j a >>= \case
		Just (j', y) -> if i' < j'
			then do	writeArray a i' y
				writeArray a j' x
				flipAll p i' j' a
			else pure (i', j')
		Nothing -> pure (i', i' - 1)

findLeft ::
	(a -> Bool) -> Int -> Int -> STArray s Int a -> ST s (Maybe (Int, a))
findLeft p i j a
	| i <= j = do
		x <- readArray a i
		bool (findLeft p (i + 1) j a) (pure $ Just (i, x)) (p x)
	| otherwise = pure Nothing

findRight ::
	(a -> Bool) -> Int -> Int -> STArray s Int a -> ST s (Maybe (Int, a))
findRight p i j a
	| i <= j = do
		x <- readArray a j
		bool (findRight p i (j - 1) a) (pure $ Just (j, x)) (p x)
	| otherwise = pure Nothing

pivot :: Ord a => Int -> Int -> STArray s Int a -> ST s a
pivot i j a | i < 0 || j < 0 =
	error $ "negative index: " ++ show i ++ " " ++ show j
pivot i j a = mid3
	<$> readArray a i <*> readArray a ((i + j) `div` 2) <*> readArray a j

mid3 :: Ord a => a -> a -> a -> a
mid3 x y z
	| x <= y = if y <= z then y else bool x z (x <= z)
	| otherwise = if x <= z then x else bool y z (y <= z)
