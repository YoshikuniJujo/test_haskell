{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BubbleSort where

import Control.Monad.ST
import Data.Array.ST
import Data.Bool

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = runST $ (>>) <$> bsort n <*> getElems =<< newListArray (1, n) xs
	where n = length xs

bsort :: Ord a => Int -> STArray s Int a -> ST s ()
bsort bound xs = swap xs 0 [1 .. bound - 1] >>= \t ->
	bool (bsort t xs) (pure ()) (t == 0)

swap :: Ord a => STArray s Int a -> Int -> [Int] -> ST s Int
swap xs t = \case
	[] -> pure t
	(j : js) -> readArray xs j >>= \xj -> readArray xs (j + 1) >>= \xj1 ->
		if xj > xj1
			then do	writeArray xs j xj1; writeArray xs (j + 1) xj
				swap xs j js
			else swap xs t js
