{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSort.Natural (naturalSort) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Array.Tools
import Data.Bool

naturalSort :: Ord a => [a] -> [a]
naturalSort ks = take n
	$ runST $ (>>) <$> nsort n False <*> getElems =<< prepareArray n ks
	where n = length ks

prepareArray :: Int -> [a] -> ST s (STArray s Int a)
prepareArray n ks = newArray_ (1, n * 2) >>= \a ->
	a <$ uncurry (writeArray a) `mapM_` zip [1 ..] ks

nsort :: Ord a => Int -> Bool -> STArray s Int a -> ST s ()
nsort n s ks = inner ks i j k l 1 True >>= bool
	(nsort n (not s) ks) (unless s $ for_ [1 .. n] \m -> copy ks m (n + m))
	where (i, j, k, l) = bool (1, n, n + 1, 2 * n) (n + 1, 2 * n, 1, n) s

inner :: Ord a =>
	STArray s Int a -> Int -> Int -> Int -> Int -> Int -> Bool -> ST s Bool
inner ks i j k l d f
	| i == j = f <$ copy ks k i
	| otherwise = readArray ks i >>= \ki -> readArray ks j >>= \kj ->
		if ki > kj
		then transR ks i j k d >>= \case
			Nothing -> inner ks i j' k' l d f
			Just (ii, kk) -> inner ks ii j' l kk (- d) False
		else transL ks i j k d >>= \case
			Nothing -> inner ks i' j k' l d f
			Just (jj, kk) -> inner ks i' jj l kk (- d) False
	where i' = i + 1; j' = j - 1; k' = k + d

transL :: Ord a =>
	STArray  s Int a -> Int -> Int -> Int -> Int -> ST s (Maybe (Int, Int))
transL ks i j k d = readArray ks i >>= \ki ->
	writeArray ks k ki >> readArray ks i' >>=
		bool (Just <$> flushR ks j k' d) (pure Nothing) . (ki <=)
	where i' = i + 1; k' = k + d

flushR :: Ord a => STArray s Int a -> Int -> Int -> Int -> ST s (Int, Int)
flushR ks j k d = readArray ks j >>= \kj ->
	writeArray ks k kj >> readArray ks j' >>=
		bool (pure (j', k')) (flushR ks j' k' d) . (kj <=)
	where j' = j - 1; k' = k + d

transR :: Ord a =>
	STArray s Int a -> Int -> Int -> Int -> Int -> ST s (Maybe (Int, Int))
transR ks i j k d = readArray ks j >>= \kj ->
	writeArray ks k kj >> readArray ks j' >>=
		bool (Just <$> flushL ks i k' d) (pure Nothing) . (kj <=)
	where j' = j - 1; k' = k + d

flushL :: Ord a => STArray s Int a -> Int -> Int -> Int -> ST s (Int, Int)
flushL ks i k d = readArray ks i >>= \ki ->
	writeArray ks k ki >> readArray ks i' >>=
		bool (pure (i', k')) (flushL ks i' k' d) . (ki <=)
	where i' = i + 1; k' = k + d
