{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SortingByCounting where

import Control.Monad.ST
import Data.Foldable
import Data.Array.ST

ccountsort :: Ord a => [a] -> [a]
ccountsort ks =
	runST $ (>>) <$> csort <*> getElems =<< newListArray (1, length ks) ks

csort :: forall s a . Ord a => STArray s Int a -> ST s ()
csort ks = do
	(_, n) <- getBounds ks
	count <- newArray (1, n) (0 :: Int)
	for_ [n, n - 1 .. 2] \i ->
		for_ [i - 1, i - 2 .. 1] \j -> do
			ki <- readArray ks i
			kj <- readArray ks j
			if ki < kj
				then modifyArray count j (+ 1)
				else modifyArray count i (+ 1)
	ks' <- newArray_ (1, n) :: ST s (STArray s Int a)
	for_ [1 .. n] \i -> do
		idx <- readArray count i
		writeArray ks' (idx + 1) =<< readArray ks i
	for_ [1 .. n] \i -> writeArray ks i =<< readArray ks' i

modifyArray :: Ix i => STArray s i a -> i -> (a -> a) -> ST s ()
modifyArray a i m = writeArray a i . m =<< readArray a i

dcountsort :: Ix a => (a, a) -> [a] -> [a]
dcountsort r ks =
	runST $ (>>) <$> dsort r <*> getElems =<< newListArray (1, length ks) ks

dsort :: forall s a . Ix a => (a, a) -> STArray s Int a -> ST s ()
dsort r ks = do
	(_, n) <- getBounds ks
	count <- newArray r 0 :: ST s (STArray s a Int)
	for_ [1 .. n] \j -> do
		kj <- readArray ks j
		modifyArray count kj (+ 1)
	for_ (range r `zip` tail (range r)) \(i1, i) -> do
		ci1 <- readArray count i1
		modifyArray count i (+ ci1)
	s <- newArray_ (1, n) :: ST s (STArray s Int a)
	for_ [n, n - 1 .. 1] \j -> do
		kj <- readArray ks j
		i <- readArray count kj
		writeArray s i kj
		writeArray count kj (i - 1)
	for_ [1 .. n] \i -> writeArray ks i =<< readArray s i
