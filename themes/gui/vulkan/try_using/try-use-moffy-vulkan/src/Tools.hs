{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (

	-- * BASIC

	clampOld, onlyIf, checkFlag, findPlusM, findBySnd,

	-- * LIST

	Inf(..), cycleI,

	-- * BITS

	checkBits,

	-- * VECTOR

	genericReplicate, indexingVector,

	-- * PICTURE

	Image(..), readRgba8

	) where

import Control.Concurrent.STM
import Data.IORef
import Data.Bits
import Data.Bool
import Data.Maybe
import Data.List hiding (genericReplicate)

import Codec.Picture

import Foreign.Storable

import qualified Data.Map as M
import qualified Data.Vector.Storable as V

toBits :: FiniteBits a => a -> [a]
toBits x =
	catMaybes $ checkBit x . (bit 0 `shiftL`) <$> [0 .. finiteBitSize x - 1]

checkBit :: FiniteBits a => a -> a -> Maybe a
checkBit bs b = bool Nothing (Just b) $ bs .&. b /= zeroBits

clampOld :: Ord a => a -> a -> a -> a
clampOld x mn mx | x < mn = mn | x < mx = x | otherwise = mx

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x | p x = Just x | otherwise = Nothing

checkFlag :: TVar Bool -> IO Bool
checkFlag fg = atomically
	$ readTVar fg >>= bool (pure False) (True <$ writeTVar fg False)

checkBits :: Bits bs => bs -> bs -> Bool
checkBits wnt = (== wnt) . (.&. wnt)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

findPlus :: (a -> Maybe b) -> [a] -> Maybe (a, b)
findPlus prd = \case
	[] -> Nothing; x : xs -> case prd x of
		Nothing -> findPlus prd xs; Just y -> Just (x, y)

findPlusM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe (a, b))
findPlusM prd = \case
	[] -> pure Nothing; x : xs -> prd x >>= \case
		Nothing -> findPlusM prd xs; Just y -> pure $ Just (x, y)

findBySnd :: (b -> Bool) -> [(a, b)] -> Maybe a
findBySnd p = (fst <$>) . find (p . snd)

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
