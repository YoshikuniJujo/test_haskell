{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryIndexing where

import Foreign.Storable
import Control.Monad.ST
import Data.STRef

import qualified Data.Map as M
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

indexing :: (Storable i, Storable a, Enum i, Ord a) =>
	V.Vector i -> V.Vector a -> (V.Vector i, V.Vector a)
indexing is xs = (is, xs)
	where
	ln = V.length is

indexingSt :: (Storable i, Storable a, Enum i, Ord a) =>
	V.Vector i -> V.Vector a -> ST s (MV.STVector s i, MV.STVector s a)
indexingSt is xs = do
	n <- newSTRef 0
	is' <- MV.new ln
	xs' <- MV.new ln
	pure (is', xs')
	where ln = V.length is

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
