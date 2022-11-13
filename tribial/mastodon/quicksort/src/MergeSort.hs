{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSort where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

naturalSort :: Ord a => [a] -> [a]
naturalSort ks = take n $ runST
	$ (>>) <$> nsort <*> getElems =<< prepareArray ks
	where
	n = length ks

prepareArray :: [a] -> ST s (STArray s Int a)
prepareArray xs = do
	a <- newArray_ (1, length xs * 2)
	a <$ uncurry (writeArray a) `mapM_` zip [1 ..] xs

nsort :: Ord a => STArray s Int a -> ST s ()
nsort ks = pure ()

testMerge1Left :: (Ord a, Num a) => [a] -> [a]
testMerge1Left ks = runST do
	a <- prepareArray'
	merge1Left a 1 n (n + 1) 1
	getElems a
	where
	prepareArray' = do
		a <- prepareArray ks
		a <$ uncurry (writeArray a) `mapM_` zip [n + 1 .. n * 2] (repeat 0)
	n = length ks

merge1Left :: Ord a => STArray  s Int a -> Int -> Int -> Int -> Int -> ST s Bool
merge1Left ks i j k d = do
	ki <- readArray ks i
	ki1 <- readArray ks (i + 1)
	writeArray ks k ki
	if ki <= ki1 then pure False else flushRight ks j (k + d) d >> pure True

flushRight :: Ord a => STArray s Int a -> Int -> Int -> Int -> ST s ()
flushRight ks j k d = do
	kj <- readArray ks j
	kj1 <- readArray ks (j - 1)
	writeArray ks k kj
	when (kj <= kj1) $ flushRight ks (j - 1) (k + d) d

testMerge1Right :: (Ord a, Num a) => [a] -> [a]
testMerge1Right ks = runST do
	a <- prepareArray'
	merge1Right a 1 n (n + 1) 1
	getElems a
	where
	prepareArray' = do
		a <- prepareArray ks
		a <$ uncurry (writeArray a) `mapM_` zip [n + 1 .. n * 2] (repeat 0)
	n = length ks

merge1Right :: Ord a => STArray s Int a -> Int -> Int -> Int -> Int -> ST s Bool
merge1Right ks i j k d = do
	kj <- readArray ks j
	kj1 <- readArray ks (j - 1)
	writeArray ks k kj
	if kj <= kj1 then pure False else flushLeft ks i (k + d) d >> pure True

flushLeft :: Ord a => STArray s Int a -> Int -> Int -> Int -> ST s ()
flushLeft ks i k d = do
	ki <- readArray ks i
	ki1 <- readArray ks (i + 1)
	writeArray ks k ki
	when (ki <= ki1) $ flushLeft ks (i + 1) (k + d) d
