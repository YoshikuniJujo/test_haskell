{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeapSort.Taocp where

import Control.Monad.ST
import Data.Array.ST
import Data.Bool

heapsort :: Ord a => [a] -> [a]
heapsort xs = runST
	$ (>>) <$> hsort <*> getElems =<< newListArray (1, length xs) xs

hsort :: Ord a => STArray s Int a -> ST s ()
hsort ks = do
	n <- rangeSize <$> getBounds ks
	algorithm ks (n `div` 2 + 1) n

algorithm :: Ord a => STArray s Int a -> Int -> Int -> ST s ()
algorithm ks l r
	| l > 1 = do
		k <- readArray ks l'
		shiftup ks k r l'
		algorithm ks l' r
	| otherwise = do
		k <- readArray ks r
		writeArray ks r =<< readArray ks 1
		if r' == 1
		then writeArray ks 1 k
		else do	shiftup ks k r' l
			algorithm ks l r'
	where
	l' = l - 1
	r' = r - 1

shiftupList :: Ord a => [a] -> a -> Int -> Int -> [a]
shiftupList xs x r j = runST do
	a <- newListArray (1, length xs) xs
	shiftup a x r j
	getElems a

shiftup :: Ord a => STArray s Int a -> a -> Int -> Int -> ST s ()
shiftup ks k r j
	| j' <= r = do
		kj' <- readArray ks j'
		j'' <- if j' < r
			then do	kj'1 <- readArray ks (j' + 1)
				pure $ bool j' (j' + 1) (kj' < kj'1)
			else pure j'
		kj'' <- readArray ks j''
		if k >= kj''
			then writeArray ks i k
			else do	writeArray ks i kj''
				shiftup ks k r j''
	| otherwise = writeArray ks i k
	where i = j; j' = 2 * j
