{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module QuickSort where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Bool
import System.Random

quickSortList :: forall a . Ord a => StdGen -> [a] -> [a]
quickSortList g xs = runST $ do
	a <- newListArray (0, length xs - 1) xs :: ST s (STArray s Int a)
	_ <- quickSort g 0 (length xs - 1) a
	getElems a

quickSort :: (MArray a e m, Ix i, Num i, Random i, Ord e) => StdGen -> i -> i -> a i e -> m StdGen
quickSort g mn mx a
--	| mn >= mx = return g
	| otherwise = do
		let	(pvi, g') = randomR (mn, mx) g
		pv <- readArray a pvi
		mmd <- separate mn mx pv a
		case mmd of
			Just md -> do
				g'' <- quickSort g' mn md a
				quickSort g'' (md + 1) mx a
			Nothing -> return g

swapArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapArray a i j = do
	x <- readArray a i
	y <- readArray a j
	writeArray a i y
	writeArray a j x

leftIndex, rightIndex :: (MArray a e m, Ix i, Num i) => (e -> Bool) -> a i e -> i -> i -> m i
leftIndex p a i j
	| i > j = return i
	| otherwise = do
		x <- readArray a i
		bool (leftIndex p a (i + 1) j) (return i) $ p x

rightIndex p a i j
	| j < i = return j
	| otherwise = do
		y <- readArray a j
		bool (rightIndex p a i (j - 1)) (return j) $ p y

separate, separate' :: (MArray a e m, Ix i, Num i, Ord e) => i -> i -> e -> a i e -> m (Maybe i)
separate mn mx p a = step mn mx
	where
	step i j = do
		i' <- leftIndex (>= p) a i j
		j' <- rightIndex (< p) a i j
		case (mn <= j', i' < j') of
			(False, _) -> separate' mn mx p a
			(_, False) -> return $ Just j'
			_ -> do	swapArray a i' j'
				step (i' + 1) (j' - 1)

separate' mn mx p a = step mn mx
	where
	step i j = do
		i' <- leftIndex (> p) a i j
		j' <- rightIndex (<= p) a i j
		case (i' <= mx, i' < j') of
			(False, _) -> return Nothing
			(_, False) -> return $ Just j'
			_ -> do	swapArray a i' j'
				step (i' + 1) (j' - 1)
