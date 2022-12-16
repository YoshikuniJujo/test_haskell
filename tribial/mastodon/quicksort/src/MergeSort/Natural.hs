{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSort.Natural (naturalSort) where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Array.ST
import Data.Bool

naturalSort :: Ord a => [a] -> [a]
naturalSort xs = take n
	$ runST $ (>>) <$> nsort n False <*> getElems =<< prepareArray n xs
	where n = length xs

prepareArray :: Int -> [a] -> ST s (STArray s Int a)
prepareArray n xs = newArray_ (1, n * 2) >>= \a ->
	a <$ uncurry (writeArray a) `mapM_` zip [1 ..] xs

nsort :: Ord a => Int -> Bool -> STArray s Int a -> ST s ()
nsort n s xs = inner xs i j k l 1 True >>= bool
	(nsort n (not s) xs) (unless s $ for_ [1 .. n] \m -> copy xs m (n + m))
	where (i, j, k, l) = bool (1, n, n + 1, 2 * n) (n + 1, 2 * n, 1, n) s

inner :: Ord a =>
	STArray s Int a -> Int -> Int -> Int -> Int -> Int -> Bool -> ST s Bool
inner xs i j k l d f
	| i == j = f <$ copy xs k i
	| otherwise = readArray xs i >>= \xi -> readArray xs j >>= \xj ->
		if xi > xj
		then transR xs i j k d >>= \case
			Nothing -> inner xs i (j - 1) (k + d) l d f
			Just (i', k') -> inner xs i' (j - 1) l k' (- d) False
		else transL xs i j k d >>= \case
			Nothing -> inner xs (i + 1) j (k + d) l d f
			Just (j', k') -> inner xs (i + 1) j' l k' (- d) False

transL :: Ord a =>
	STArray  s Int a -> Int -> Int -> Int -> Int -> ST s (Maybe (Int, Int))
transL xs i j k d = readArray xs i >>= \xi ->
	writeArray xs k xi >> readArray xs i' >>=
		bool (Just <$> flushR xs j k' d) (pure Nothing) . (xi <=)
	where i' = i + 1; k' = k + d

flushR :: Ord a => STArray s Int a -> Int -> Int -> Int -> ST s (Int, Int)
flushR xs j k d = readArray xs j >>= \kj ->
	writeArray xs k kj >> readArray xs j' >>=
		bool (pure (j', k')) (flushR xs j' k' d) . (kj <=)
	where j' = j - 1; k' = k + d

transR :: Ord a =>
	STArray s Int a -> Int -> Int -> Int -> Int -> ST s (Maybe (Int, Int))
transR xs i j k d = readArray xs j >>= \xj ->
	writeArray xs k xj >> readArray xs j' >>=
		bool (Just <$> flushL xs i k' d) (pure Nothing) . (xj <=)
	where j' = j - 1; k' = k + d

flushL :: Ord a => STArray s Int a -> Int -> Int -> Int -> ST s (Int, Int)
flushL xs i k d = readArray xs i >>= \xi ->
	writeArray xs k xi >> readArray xs i' >>=
		bool (pure (i', k')) (flushL xs i' k' d) . (xi <=)
	where i' = i + 1; k' = k + d

copy :: STArray s Int a -> Int -> Int -> ST s ()
copy a d s = writeArray a d =<< readArray a s
