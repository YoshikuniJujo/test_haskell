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

merge1Left :: Ord a =>
	STArray  s Int a -> Int -> Int -> Int -> Int -> ST s (Maybe (Int, Int))
merge1Left ks i j k d = do
	ki <- readArray ks i
	ki1 <- readArray ks (i + 1)
	writeArray ks k ki
	if ki <= ki1 then pure Nothing else Just <$> flushRight ks j (k + d) d

flushRight :: Ord a => STArray s Int a -> Int -> Int -> Int -> ST s (Int, Int)
flushRight ks j k d = do
	kj <- readArray ks j
	kj1 <- readArray ks (j - 1)
	writeArray ks k kj
	if kj <= kj1
		then flushRight ks (j - 1) (k + d) d else pure (j - 1, k + d)

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

merge1Right :: Ord a =>
	STArray s Int a -> Int -> Int -> Int -> Int -> ST s (Maybe (Int, Int))
merge1Right ks i j k d = do
	kj <- readArray ks j
	kj1 <- readArray ks (j - 1)
	writeArray ks k kj
	if kj <= kj1 then pure Nothing else Just <$> flushLeft ks i (k + d) d

flushLeft :: Ord a => STArray s Int a -> Int -> Int -> Int -> ST s (Int, Int)
flushLeft ks i k d = do
	ki <- readArray ks i
	ki1 <- readArray ks (i + 1)
	writeArray ks k ki
	if ki <= ki1
		then flushLeft ks (i + 1) (k + d) d else pure (i + 1, k + d)
