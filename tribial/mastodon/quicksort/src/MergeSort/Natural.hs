{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSort.Natural (naturalSort) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Bool

naturalSort :: Ord a => [a] -> [a]
naturalSort ks =
	take n $ runST $ (>>) <$> nsort n False <*> getElems =<< prepareArray n ks
	where n = length ks

prepareArray :: Int -> [a] -> ST s (STArray s Int a)
prepareArray n xs = newArray_ (1, n * 2) >>= \a ->
	a <$ uncurry (writeArray a) `mapM_` zip [1 ..] xs

nsort :: Ord a => Int -> Bool -> STArray s Int a -> ST s ()
nsort n s ks = inner ks i j k l 1 True >>= bool
	(nsort n (not s) ks)
	(unless s $ for_ [1 .. n] \m -> copy ks m (n + m))
	where (i, j, k, l) = bool (1, n, n + 1, 2 * n) (n + 1, 2 * n, 1, n) s

copy :: STArray s Int a -> Int -> Int -> ST s ()
copy a d s = writeArray a d =<< readArray a s

inner :: Ord a =>
	STArray s Int a -> Int -> Int -> Int -> Int -> Int -> Bool -> ST s Bool
inner ks i j k l d f
	| i == j = f <$ copy ks k i
	| otherwise = readArray ks i >>= \ki -> readArray ks j >>= \kj ->
		if ki > kj
			then merge1Right ks i j k d >>= \case
				Nothing -> inner ks i (j - 1) (k + d) l d f
				Just (i', k') ->
					inner ks i' (j - 1) l k' (- d) False
			else merge1Left ks i j k d >>= \case
				Nothing -> inner ks (i + 1) j (k + d) l d f
				Just (j', k') ->
					inner ks (i + 1) j' l k' (- d) False

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
