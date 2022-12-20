{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StraightSelectionSort where

import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Bool

selectionSort :: Ord a => [a] -> [a]
selectionSort ks =
	runST $ (>>) <$> ssort n <*> getElems =<< newListArray (1, n) ks
	where n = length ks

ssort :: Ord a => Int -> STArray s Int a -> ST s ()
ssort n ks = for_ [n, n - 1 .. 2] \j -> findMax ks j (j - 1) >>= \i -> do
	ki <- readArray ks i; kj <- readArray ks j
	writeArray ks i kj; writeArray ks j ki

findMax :: Ord a => STArray s Int a -> Int -> Int -> ST s Int
findMax ks i k = readArray ks i >>= \ki -> readArray ks k >>= \kk -> do
	let	i' = bool k i (ki >= kk)
	bool (pure i') (findMax ks i' (k - 1)) (k > 1)
