{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SortingByCounting where

import Control.Monad.ST
import Data.Foldable
import Data.Array.ST

countsort :: Ord a => [a] -> [a]
countsort ks =
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
	for_ [1 .. n] \i -> do
		writeArray ks i =<< readArray ks' i

modifyArray :: STArray s Int a -> Int -> (a -> a) -> ST s ()
modifyArray a i m = writeArray a i . m =<< readArray a i
