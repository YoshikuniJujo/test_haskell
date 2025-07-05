{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Filters (filter, unfilter) where

import Prelude hiding (filter)
import Control.Arrow
import Data.Foldable
import Data.MonoTraversable
import Data.Function
import Data.Either
import Data.List qualified as L
import Data.Word
import Data.Int
import Data.ByteString.FingerTree qualified as BSF

unfilter :: Int -> [Word8] -> BSF.ByteString -> Either String [Word8]
unfilter bpp prior (BSF.uncons -> filtered) = case filtered of
	Nothing -> Left "empty line"
	Just (ft, fs) -> case ft of
		0 -> Right $ otoList fs
		1 -> Right $ unfilterSub (BSF.replicate bpp 0) (otoList fs)
		2 -> Right $ unfilterUp prior fs
		3 -> Right $ unfilterAverage prior (BSF.replicate bpp 0) fs
		4 -> Right $ unfilterPaeth (BSF.replicate bpp 0) (otoList prior) (BSF.replicate bpp 0) (otoList fs)
		_ -> Left $ "unknown filter type: " ++ show ft ++ " " ++ show fs

filter :: Int -> [Word8] -> BSF.ByteString -> [Word8]
filter bpp prior raw = minimumBy (compare `on` calc) . rights $ filter' bpp prior raw <$> [0 .. 4]

filter' :: Int -> [Word8] -> BSF.ByteString -> Word8 -> Either String [Word8]
filter' bpp prior raw = \case
	0 -> Right $ 0 : BSF.unpack raw
	1 -> Right $ 1 : filterSub (BSF.replicate bpp 0) (otoList raw)
	2 -> Right $ 2 : filterUp prior raw
	3 -> Right $ 3 : filterAverage prior (BSF.replicate bpp 0) raw
	4 -> Right $ 4 : filterPaeth
		(BSF.replicate bpp 0) (otoList prior)
		(BSF.replicate bpp 0) (otoList raw)
	ft -> Left $ "unknown filter type: " ++ show ft

calc :: [Word8] -> Int
calc = sum . ((^ (2 :: Int)) . fromIntegral . fromIntegral @_ @Int8 <$>)

unfilterSub :: BSF.ByteString -> [Word8] -> [Word8]
unfilterSub (BSF.uncons -> raw) (L.uncons -> sub) = case (raw, sub) of
	(Nothing, _) -> error "never occur"
	(_, Nothing) -> []
	(Just (r, rs), Just (s, ss)) ->
		r' : unfilterSub (rs `BSF.snoc` r') ss
		where r' = r + s

filterSub :: BSF.ByteString -> [Word8] -> [Word8]
filterSub (BSF.uncons -> pr) (L.uncons -> cr) = case (pr, cr) of
	(Nothing, _) -> error "never occur"
	(_, Nothing) -> []
	(Just (r, rs), Just (s, ss)) ->
		r' : filterSub (rs `BSF.snoc` s) ss
		where r' = s - r

unfilterUp :: [Word8] -> BSF.ByteString -> [Word8]
unfilterUp = curry $ uncurry (zipWith (+)) . (id *** otoList)

filterUp :: [Word8] -> BSF.ByteString -> [Word8]
filterUp = curry $ uncurry (zipWith subtract) . (id *** otoList)

unfilterAverage ::
	[Word8] -> BSF.ByteString -> BSF.ByteString -> [Word8]
unfilterAverage (L.uncons -> prior) (BSF.uncons -> raw) (BSF.uncons -> average) =
	case (prior, raw, average) of
		(_, Nothing, _) -> error "never occur"
		(Nothing, _, Nothing) -> []
		(Just (p, ps), Just (r, rs), Just (a, as)) ->
			r' : unfilterAverage ps (rs `BSF.snoc` r') as
			where
			r' = fromIntegral ((fromIntegral p + fromIntegral r) `div` 2 :: Int) + a
		_ -> error "never occur"

filterAverage ::
	[Word8] -> BSF.ByteString -> BSF.ByteString -> [Word8]
filterAverage (L.uncons -> up) (BSF.uncons -> lft) (BSF.uncons -> cr) =
	case (up, lft, cr) of
		(_, Nothing, _) -> error "never occur"
		(Nothing, _, Nothing) -> []
		(Just (u, us), Just (l, ls), Just (c, cs)) ->
			r : filterAverage us (ls `BSF.snoc` c) cs
			where
			r = c - fromIntegral ((fromIntegral u + fromIntegral l) `div` 2 :: Int)
		_ -> error $ "never occur: filterAverage _: " ++ show up ++ " " ++ show lft ++ " " ++ show cr

unfilterPaeth :: BSF.ByteString -> [Word8] -> BSF.ByteString -> [Word8] -> [Word8]
unfilterPaeth (BSF.uncons -> priorRaw)
	(L.uncons -> prior) (BSF.uncons -> raw) (L.uncons -> paeth) =
	case (priorRaw, prior, raw, paeth) of
		(Nothing, _, _, _) -> error "never occur"
		(_, _, Nothing, _) -> error "never occur"
		(_, Nothing, _, Nothing) -> []
		(Just (pr, prs), Just (p, ps), Just (r, rs), Just (a, as)) ->
			r' : unfilterPaeth
				(prs BSF.:> p) ps (rs BSF.:> r') as
			where
			r' = a + paethPredictor r p pr
		_ -> error "never occur"

filterPaeth :: BSF.ByteString -> [Word8] -> BSF.ByteString -> [Word8] -> [Word8]
filterPaeth (BSF.uncons -> uplft)
	(L.uncons -> up) (BSF.uncons -> lft) (L.uncons -> cr) =
	case (uplft, up, lft, cr) of
		(Nothing, _, _, _) -> error "never occur"
		(_, _, Nothing, _) -> error "never occur"
		(_, Nothing, _, Nothing) -> []
		(Just (ul, uls), Just (u, us), Just (l, ls), Just (c, cs)) ->
			r : filterPaeth
				(uls BSF.:> u) us (ls BSF.:> c) cs
			where
			r = c - paethPredictor l u ul
		_ -> error "never occur"

paethPredictor :: Word8 -> Word8 -> Word8 -> Word8
paethPredictor (id &&& fromIntegral -> (a, a'))
	(id &&& fromIntegral -> (b, b')) (id &&& fromIntegral -> (c, c'))
	-- a = left, b = above, c = upper left
	| pa <= pb && pa <= pc = a
	| pb <= pc = b
	| otherwise = c
	where
	p = a' + b' - c' :: Int
	pa = abs $ p - a'
	pb = abs $ p - b'
	pc = abs $ p - c'
