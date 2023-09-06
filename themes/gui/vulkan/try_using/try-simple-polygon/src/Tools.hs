{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (

	-- * BASIC

	clamp, onlyIf, checkFlag, findPlusM, findBySnd,

	-- * LIST

	Inf(..), cycleI,

	-- * BITS

	checkBits,

	-- * VECTOR

	genericReplicate, indexingVector,

	-- * PICTURE

	Image(..), readRgba8

	) where

import Data.IORef
import Data.Bits
import Data.Bool
import Data.List qualified as L

import Codec.Picture

import Foreign.Storable

import qualified Data.Map as M
import qualified Data.Vector.Storable as V

clamp :: Ord a => a -> a -> a -> a
clamp x mn mx | x < mn = mn | x < mx = x | otherwise = mx

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x | p x = Just x | otherwise = Nothing

checkFlag :: IORef Bool -> IO Bool
checkFlag fg = readIORef fg >>= bool (pure False) (True <$ writeIORef fg False)

checkBits :: Bits bs => bs -> bs -> Bool
checkBits wnt = (== wnt) . (.&. wnt)

findPlusM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe (a, b))
findPlusM prd = \case
	[] -> pure Nothing; x : xs -> prd x >>= \case
		Nothing -> findPlusM prd xs; Just y -> pure $ Just (x, y)

findBySnd :: (b -> Bool) -> [(a, b)] -> Maybe a
findBySnd p = (fst <$>) . L.find (p . snd)

data Inf a = a :- Inf a deriving Show

cycleI :: [a] -> Inf a
cycleI xs = go xs where go = \case [] -> cycleI xs; y : ys -> y :- go ys

readRgba8 :: FilePath -> IO (Image PixelRGBA8)
readRgba8 fp = either error convertRGBA8 <$> readImage fp

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
