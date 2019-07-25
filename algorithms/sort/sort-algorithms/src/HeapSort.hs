{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeapSort where

import Control.Monad.ST
import Data.Foldable
import Data.Array.MArray
import Data.Array.ST
import Data.Bool

heapSort :: forall a . Ord a => [a] -> [a]
heapSort xs = runST $ (>>) <$> heapSortArray <*> getElems
	=<< (newListArray (0, length xs - 1) xs :: ST s (STArray s Int a))

heapSortArray :: (MArray a e m, Ix i, Integral i, Ord e) => a i e -> m ()
heapSortArray = (>>) <$> construct <*> destruct

construct :: (MArray a e m, Ix i, Integral i, Ord e) => a i e -> m ()
construct arr = do
	(i0, ix) <- getBounds arr
	flip (correct arr i0) ix `mapM_` reverse (range (i0, parent i0 ix))

destruct :: (MArray a e m, Ix i, Integral i, Ord e) => a i e -> m ()
destruct arr = do
	bs@(i0, _) <- getBounds arr
	for_ (init . reverse $ range bs)
		$ (>>) <$> swap arr i0 <*> correct arr i0 i0 . subtract 1

correct ::
	(MArray a e m, Ix i, Integral i, Ord e) => a i e -> i -> i -> i -> m ()
correct arr i0 b e
	| li > e = return ()
	| ri > e = do
		p <- arr `readArray` b
		lc <- arr `readArray` li
		bool (return ()) (swap arr b li >> correct arr i0 li e) (lc > p)
	| otherwise = do
		p <- arr `readArray` b
		lc <- arr `readArray` li
		rc <- arr `readArray` ri
		if p >= lc && p >= rc then return () else bool
			(swap arr b li >> correct arr i0 li e)
			(swap arr b ri >> correct arr i0 ri e)
			(lc < rc)
	where li = leftChild i0 b; ri = rightChild i0 b

parent, leftChild, rightChild :: Integral i => i -> i -> i
parent i0 i = (i + i0 - 1) `div` 2
leftChild i0 i = i * 2 - i0 + 1
rightChild i0 i = i * 2 - i0 + 2

swap :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swap arr i j = do
	x <- arr `readArray` i
	y <- arr `readArray` j
	(arr `writeArray` i) y
	(arr `writeArray` j) x
