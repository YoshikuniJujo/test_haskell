{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StraightInsertionSort (insertionSort) where

import Control.Monad.ST
import Data.Foldable
import Data.Array.ST

insertionSort :: Ord a => [a] -> [a]
insertionSort xs = runST
	$ (>>) <$> isort <*> getElems =<< newListArray (1, length xs) xs

isort :: Ord a => STArray s Int a -> ST s ()
isort xs = do
	(_, n) <- getBounds xs
	for_ [2 .. n] \j -> do
		xj <- readArray xs j
		insert xs (j - 1) xj

insert :: Ord a => STArray s Int a -> Int -> a -> ST s ()
insert xs i x = do
	xi <- readArray xs i
	if x >= xi
		then writeArray xs (i + 1) x
		else do	writeArray xs (i + 1) xi
			if i - 1 > 0
				then insert xs (i - 1) x
				else writeArray xs i x
