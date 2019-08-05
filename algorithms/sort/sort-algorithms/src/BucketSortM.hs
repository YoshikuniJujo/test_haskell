{-# LANGUAGE
	FlexibleContexts, ScopedTypeVariables,
	TypeApplications, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BucketSortM (bucketSortM, bsortM) where

import Data.Foldable
import Data.Array.MArray

import MArrayTools

bucketSortM :: forall a m i . (MArray a Int m, Ix i) => (i, i) -> [i] -> m [i]
bucketSortM bs is = bucketSortResult =<< bucketSortMArray @a bs is

bucketSortMArray :: (MArray a Int m, Ix i) => (i, i) -> [i] -> m (a i Int)
bucketSortMArray bs is = do
	a <- newArray bs 0
	flip (modifyArray a) (+ 1) `mapM_` is
	return a

bucketSortResult :: (MArray a Int m, Ix i) => a i Int -> m [i]
bucketSortResult a = (concat <$>)
	$ mapM (\i -> (`replicate` i) <$> readArray a i) . range =<< getBounds a

bsortM :: forall a a' m i x . (MArray a Int m, MArray a' x m, Ix i) =>
	(x -> i) -> (i, i) -> [x] -> m [x]
bsortM getIx bs xs = getElems
	=<< bucketSortStep2 @a @a' getIx xs
	=<< bucketSortMArray bs (getIx <$> xs)

bucketSortStep2 :: forall a a' m i x . (MArray a Int m, Ix i, MArray a' x m) =>
	(x -> i) -> [x] -> a i Int -> m (a' Int x)
bucketSortStep2 getIx lst ns = do
	bs <- getBounds ns
	(jsgen, len) <- scanMArray (+) ns 0
	js <- newListArray bs jsgen :: m (a i Int)
	xs <- newArray_ (0, len - 1)
	for_ lst $ \x -> do
		let	i = getIx x
		j <- readArray js i
		modifyArray js i (+ 1)
		writeArray xs j x
	return xs
