{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Filters (unfilter) where

import Control.Arrow
import Data.Foldable
import Data.Word
import Data.List qualified as L
import Data.Sequence qualified as Seq
import Data.Sequence.ToolsYj qualified as Seq

unfilter :: Int -> [Word8] -> Seq.Seq Word8 -> Either String [Word8]
unfilter bpp prior (Seq.uncons -> filtered) = case filtered of
	Nothing -> Left "empty line"
	Just (ft, fs) -> case ft of
		0 -> Right $ toList fs
		1 -> Right $ unfilterSub (Seq.replicate bpp 0) (toList fs)
		2 -> Right $ unfilterUp prior fs
		3 -> Right $ unfilterAverage prior (Seq.replicate bpp 0) fs
		4 -> Right $ unfilterPaeth (Seq.replicate bpp 0) (toList prior) (Seq.replicate bpp 0) (toList fs)
		_ -> Left "unknown filter type"

unfilterSub :: Seq.Seq Word8 -> [Word8] -> [Word8]
unfilterSub (Seq.uncons -> raw) (L.uncons -> sub) = case (raw, sub) of
	(Nothing, _) -> error "never occur"
	(_, Nothing) -> []
	(Just (r, rs), Just (s, ss)) ->
		r' : unfilterSub (rs `Seq.snoc` r') ss
		where r' = r + s

unfilterUp :: [Word8] -> Seq.Seq Word8 -> [Word8]
unfilterUp = curry $ uncurry (zipWith (+)) . (id *** toList)

unfilterAverage ::
	[Word8] -> Seq.Seq Word8 -> Seq.Seq Word8 -> [Word8]
unfilterAverage (L.uncons -> prior) (Seq.uncons -> raw) (Seq.uncons -> average) =
	case (prior, raw, average) of
		(_, Nothing, _) -> error "never occur"
		(Nothing, _, Nothing) -> []
		(Just (p, ps), Just (r, rs), Just (a, as)) ->
			r' : unfilterAverage ps (rs `Seq.snoc` r') as
			where
			r' = fromIntegral ((fromIntegral p + fromIntegral r) `div` 2 :: Int) + a
		_ -> error "never occur"

unfilterPaeth :: Seq.Seq Word8 -> [Word8] -> Seq.Seq Word8 -> [Word8] -> [Word8]
unfilterPaeth (Seq.uncons -> priorRaw)
	(L.uncons -> prior) (Seq.uncons -> raw) (L.uncons -> paeth) =
	case (priorRaw, prior, raw, paeth) of
		(Nothing, _, _, _) -> error "never occur"
		(_, _, Nothing, _) -> error "never occur"
		(_, Nothing, _, Nothing) -> []
		(Just (pr, prs), Just (p, ps), Just (r, rs), Just (a, as)) ->
			r' : unfilterPaeth
				(prs Seq.:|> p) ps (rs Seq.:|> r') as
			where
			r' = a + paethPredictor r p pr
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
