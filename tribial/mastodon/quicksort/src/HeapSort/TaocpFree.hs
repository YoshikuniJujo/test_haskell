{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeapSort.TaocpFree (heapsort) where

import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Bool

heapsort :: Ord a => [a] -> [a]
heapsort ks = runST $ (>>) <$> hsort m n <*> getElems =<< newListArray (1, n) ks
	where m = n `div` 2; n = length ks

hsort :: Ord a => Int -> Int -> STArray s Int a -> ST s ()
hsort m n ks = do
	for_ [m, m - 1 .. 1] \l -> shiftup ks l n =<< readArray ks l
	for_ [n, n - 1 .. 2] \r -> readArray ks r >>= \k -> do
		writeArray ks r =<< readArray ks 1
		shiftup ks 1 (r - 1) k

shiftup :: Ord a => STArray s Int a -> Int -> Int -> a -> ST s ()
shiftup ks j r k
	| cl <= r = do
		kcl <- readArray ks cl
		(cb, kcb) <- if cl < r
			then do	kcr <- readArray ks cr
				pure $ bool (cl, kcl) (cr, kcr) (kcl < kcr)
			else pure (cl, kcl)
		if k >= kcb
			then writeArray ks j k
			else writeArray ks j kcb >> shiftup ks cb r k
	| otherwise = writeArray ks j k
	where cl = 2 * j; cr = cl + 1
