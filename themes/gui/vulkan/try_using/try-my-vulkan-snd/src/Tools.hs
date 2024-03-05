{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools {-# DEPRECATED "use package try-tools-yj" #-} (

	-- * BASIC

	clampOld, onlyIf, checkFlag, findPlusM, findBySnd,

	-- * LIST

	Inf(..), cycleI,

	-- * BITS

	checkBits,

	-- * VECTOR

	genericReplicate, indexing, indexingVector,

	-- * PICTURE

	Image(..), readRgba8

	) where

import Data.IORef
import Data.Bits
import Data.Bool
import Data.List hiding (genericReplicate)

import Codec.Picture

import Foreign.Storable

import qualified Data.Map as M
import qualified Data.Vector.Storable as V

import qualified Data.Sequences as Seq
import qualified Data.MonoTraversable as Seq

clampOld :: Ord a => a -> a -> a -> a
clampOld x mn mx | x < mn = mn | x < mx = x | otherwise = mx

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
indexingVector = indexing

indexing :: (
	Seq.MonoFoldable seq, Seq.Element seq ~ Seq.Element seq',
	Seq.IsSequence seq', Ord (Seq.Element seq),
	Seq.IsSequence seqi, Enum (Seq.Element seqi) ) => seq -> (seq', seqi)
indexing (Seq.otoList -> xs) = let
	(xs', is) = indexingGo 0 M.empty xs in
	(Seq.fromList $ M.elems xs', Seq.fromList $ toEnum <$> is)

indexingGo :: Ord a => Int -> M.Map a Int -> [a] -> (M.Map Int a, [Int])
indexingGo _ _ [] = (M.empty, [])
indexingGo idx rvdct (x : xs) = case M.lookup x rvdct of
	Nothing -> let
		(dct, is) = indexingGo (idx + 1) (M.insert x idx rvdct) xs in
		(M.insert idx x dct, idx : is)
	Just i -> let (dct, is) = indexingGo idx rvdct xs in (dct, i : is)
