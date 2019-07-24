{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeapSort where

import Control.Monad.ST
import Data.Foldable
import Data.Array.MArray
import Data.Array.ST
import Data.Bool

swapArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapArray arr i j = do
	x <- arr `readArray` i
	y <- arr `readArray` j
	(arr `writeArray` i) y
	(arr `writeArray` j) x

parent :: Integral i => i -> i
parent i = (i + 1) `div` 2 - 1

leftChild, rightChild :: Integral i => i -> i
leftChild i = (i + 1) * 2 - 1
rightChild i = (i + 1) * 2

heapSortList :: forall a . Ord a => [a] -> [a]
heapSortList xs = runST $ do
	a <- newListArray (0, length xs - 1) xs :: ST s (STArray s Int a)
	heapSort a
	getElems a

heapSort :: (MArray a e m, Ix i, Integral i, Ord e) => a i e -> m ()
heapSort = (>>) <$> makeTree <*> fromTree

makeTree :: (MArray a e m, Ix i, Integral i, Ord e) => a i e -> m ()
makeTree arr = do
	bs <- getBounds arr
	mapM_ (arr `upheap`) . tail $ range bs

fromTree :: (MArray a e m, Ix i, Integral i, Ord e) => a i e -> m ()
fromTree arr = do
	bs <- getBounds arr
	for_ (init . reverse $ range bs) $ \i -> do
		swapArray arr 0 i
		downheap arr i 0

upheap :: (MArray a e m, Ix i, Integral i, Ord e) => a i e -> i -> m ()
upheap _ n | n < 1 = return ()
upheap arr n = do
	p <- arr `readArray` m
	c <- arr `readArray` n
	bool (return ()) (swapArray arr m n >> upheap arr m) (p < c)
	where m = parent n

downheap :: (MArray a e m, Ix i, Integral i, Ord e) => a i e -> i -> i -> m ()
downheap arr n m
	| lc >= n = return ()
	| rc >= n = do
		p <- arr `readArray` m
		l <- arr `readArray` lc
		bool (return ()) (swapArray arr lc m >> downheap arr n lc) (l > p)
	| otherwise = do
		p <- arr `readArray` m
		l <- arr `readArray` lc
		r <- arr `readArray` rc
		if p >= l && p >= r then return () else
			bool	(swapArray arr lc m >> downheap arr n lc)
				(swapArray arr rc m >> downheap arr n rc)
				(l < r)
	where
	lc = leftChild m
	rc = rightChild m
