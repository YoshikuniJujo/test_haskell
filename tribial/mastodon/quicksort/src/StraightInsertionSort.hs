{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StraightInsertionSort (insertionSort, isort) where

import Control.Monad.ST
import Data.Foldable
import Data.Array.ST

insertionSort :: Ord a => [a] -> [a]
insertionSort xs =
	runST $ (>>) <$> isort n <*> getElems =<< newListArray (1, n) xs
	where n = length xs

isort :: Ord a => Int -> STArray s Int a -> ST s ()
isort n xs = for_ [2 .. n] \j -> insert xs (j - 1) =<< readArray xs j

insert :: Ord a => STArray s Int a -> Int -> a -> ST s ()
insert xs i x = readArray xs i >>= \xi -> if x >= xi
	then writeArray xs (i + 1) x
	else do	writeArray xs (i + 1) xi
		if i - 1 > 0 then insert xs (i - 1) x else writeArray xs i x
