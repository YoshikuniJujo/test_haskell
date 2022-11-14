{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSort where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Bool

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
nsort ks = do
	(_, n) <- getBounds ks
	run ks (n `div` 2) False

run :: Ord a => STArray s Int a -> Int -> Bool -> ST s ()
run ks n s = do
	end <- inner ks i j k l 1 True
	if end	then for_ [1 .. n] \m ->
			unless s $ writeArray ks i =<< readArray ks (n + m)
		else run ks n (not s)
	where (i, j, k, l) = bool (1, n, n + 1, 2 * n) (n + 1, 2 * n, 1, n) s

inner :: Ord a => STArray s Int a ->
	Int -> Int -> Int -> Int -> Int -> Bool -> ST s Bool
inner ks i j k l d f
	| i == j = do
		writeArray ks k =<< readArray ks i
		pure f
	| otherwise = do
		ki <- readArray ks i
		kj <- readArray ks j
		pure True
		if ki > kj
			then merge1Right ks i j k d >>= \case
				Nothing -> inner ks i (j - 1) (k + d) l d f
				Just (i', k') ->
					inner ks i' (j - 1) l k' (- d) False
			else merge1Left ks i j k d >>= \case
				Nothing -> inner ks (i + 1) j (k + d) l d f
				Just (j', k') ->
					inner ks (i + 1) j' l k' (- d) False

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
