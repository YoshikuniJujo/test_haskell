{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StraightSelectionSort where

import Control.Monad.ST
import Data.Foldable
import Data.Array.ST

selectionSort :: Ord a => [a] -> [a]
selectionSort xs =
	runST $ (>>) <$> ssort n <*> getElems =<< newListArray (1, n) xs
	where n = length xs

ssort :: Ord a => Int -> STArray s Int a -> ST s ()
ssort n xs = for_ [n, n - 1 .. 2] \j -> readArray xs j >>= \xj -> do
	(i, xi) <- findMax xs j xj (j - 1)
	writeArray xs i xj >> writeArray xs j xi

findMax :: Ord a => STArray s Int a -> Int -> a -> Int -> ST s (Int, a)
findMax xs i ra k = readArray xs k >>= \xk -> do
	let	(i', ra') = if ra >= xk then (i, ra) else (k, xk)
	if (k > 1) then findMax xs i' ra' (k - 1) else pure (i', ra')
