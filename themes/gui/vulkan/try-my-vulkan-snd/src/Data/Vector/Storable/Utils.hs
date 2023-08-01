{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Vector.Storable.Utils (genericReplicate, indexingVector) where

import Foreign.Storable

import qualified Data.Map as M
import qualified Data.Vector.Storable as V

genericReplicate :: (Integral i, Storable a) => i -> a -> V.Vector a
genericReplicate = V.replicate . fromIntegral

indexingVector :: (Storable i, Enum i, Storable a, Ord a) =>
	V.Vector a -> (V.Vector a, V.Vector i)
indexingVector (V.toList -> xs) = let
	(xs', is) = indexingList 0 M.empty xs in
	(V.fromList $ M.elems xs', V.fromList $ toEnum <$> is)

indexingList :: Ord a => Int -> M.Map a Int -> [a] -> (M.Map Int a, [Int])
indexingList _ _ [] = (M.empty, [])
indexingList idx rvdct (x : xs) = case M.lookup x rvdct of
	Just i -> let	(dct, is) = indexingList idx rvdct xs in
			(dct, i : is)
	Nothing -> let	(dct, is) = indexingList (idx + 1) (M.insert x idx rvdct) xs in
			(M.insert idx x dct, idx : is)
