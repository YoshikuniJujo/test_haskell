{-# LANGUAGE
	FlexibleContexts, ScopedTypeVariables,
	TypeApplications, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BucketSortM (bucketSortM) where

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
