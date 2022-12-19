{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StraightSelectionSort where

import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Bool

selectionSort :: Ord a => [a] -> [a]
selectionSort xs =
	runST $ (>>) <$> ssort n <*> getElems =<< newListArray (1, n) xs
	where n = length xs

ssort :: Ord a => Int -> STArray s Int a -> ST s ()
ssort n xs = for_ [n, n - 1 .. 2] \j -> findMax xs j (j - 1) >>= \i -> do
	xi <- readArray xs i; xj <- readArray xs j
	writeArray xs i xj; writeArray xs j xi

findMax :: Ord a => STArray s Int a -> Int -> Int -> ST s Int
findMax xs i k = readArray xs i >>= \xi -> readArray xs k >>= \xk -> do
	let	i' = bool k i (xi >= xk)
	bool (pure i') (findMax xs i' (k - 1)) (k > 1)
