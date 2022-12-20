{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StraightInsertionSort (insertionSort, isort) where

import Control.Monad.ST
import Data.Foldable
import Data.Array.ST

insertionSort :: Ord a => [a] -> [a]
insertionSort ks =
	runST $ (>>) <$> isort n <*> getElems =<< newListArray (1, n) ks
	where n = length ks

isort :: Ord a => Int -> STArray s Int a -> ST s ()
isort n ks = for_ [2 .. n] \j -> insert ks (j - 1) =<< readArray ks j

insert :: Ord a => STArray s Int a -> Int -> a -> ST s ()
insert ks i k = readArray ks i >>= \ki -> if k >= ki
	then writeArray ks (i + 1) k
	else do	writeArray ks (i + 1) ki
		if i - 1 > 0 then insert ks (i - 1) k else writeArray ks i k
