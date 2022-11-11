{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SortingByInsertion where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Array.ST

sinsertionsort :: Ord a => [a] -> [a]
sinsertionsort ks =
	runST $ (>>) <$> ssort <*> getElems =<< newListArray (1, length ks) ks

ssort :: Ord a => STArray s Int a -> ST s ()
ssort ks = do
	(_, n) <- getBounds ks
	for_ [2 .. n] \j -> do
		k <- readArray ks j
		ssort1 ks (j - 1) k

ssort1 :: Ord a => STArray s Int a -> Int -> a -> ST s ()
ssort1 ks i k = do
	ki <- readArray ks i
	if k >= ki then writeArray ks (i + 1) k else do
		writeArray ks (i + 1) =<< readArray ks i
		if i > 1 then ssort1 ks (i - 1) k else writeArray ks i k
