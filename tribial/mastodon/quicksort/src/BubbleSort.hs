{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BubbleSort where

import Control.Monad.ST
import Data.Array.ST
import Data.Bool

bubbleSort :: Ord a => [a] -> [a]
bubbleSort ks = runST $ (>>) <$> bsort n <*> getElems =<< newListArray (1, n) ks
	where n = length ks

bsort :: Ord a => Int -> STArray s Int a -> ST s ()
bsort bound ks = swap ks 0 [1 .. bound - 1] >>= \t ->
	bool (bsort t ks) (pure ()) (t == 0)

swap :: Ord a => STArray s Int a -> Int -> [Int] -> ST s Int
swap ks t = \case
	[] -> pure t
	(j : js) -> readArray ks j >>= \kj -> readArray ks (j + 1) >>= \kj1 ->
		if kj > kj1
			then do	writeArray ks j kj1; writeArray ks (j + 1) kj
				swap ks j js
			else swap ks t js
