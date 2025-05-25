{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Filters where

import Control.Arrow
import Data.Word
import Data.ByteString qualified as BS

unfilter :: Int -> BS.ByteString -> BS.ByteString -> Either String BS.ByteString
unfilter bpp prior (BS.uncons -> filtered) = case filtered of
	Nothing -> Left "empty line"
	Just (ft, fs) -> case ft of
		0 -> Right fs
		1 -> Right $ unfilterSub (BS.replicate bpp 0) fs
		2 -> Right $ unfilterUp prior fs
		3 -> Right $ unfilterAverage prior (BS.replicate bpp 0) fs
		4 -> Right $ unfilterPaeth
			(BS.replicate bpp 0) prior (BS.replicate bpp 0) fs
		_ -> Left "unknown filter type"

unfilterSub :: BS.ByteString -> BS.ByteString -> BS.ByteString
unfilterSub (BS.uncons -> raw) (BS.uncons -> sub) = case (raw, sub) of
	(Nothing, _) -> error "never occur"
	(_, Nothing) -> ""
	(Just (r, rs), Just (s, ss)) ->
		r' `BS.cons` unfilterSub (rs `BS.snoc` r') ss
		where r' = r + s

unfilterUp :: BS.ByteString -> BS.ByteString -> BS.ByteString
unfilterUp = BS.packZipWith (+)

unfilterAverage ::
	BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
unfilterAverage (BS.uncons -> prior) (BS.uncons -> raw) (BS.uncons -> average) =
	case (prior, raw, average) of
		(_, Nothing, _) -> error "never occur"
		(Nothing, _, Nothing) -> ""
		(Just (p, ps), Just (r, rs), Just (a, as)) ->
			r' `BS.cons` unfilterAverage ps (rs `BS.snoc` r') as
			where
			r' = ((p + r) `div` 2) + a
		_ -> error "never occur"

unfilterPaeth :: BS.ByteString ->
	BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
unfilterPaeth (BS.uncons -> priorRaw)
	(BS.uncons -> prior) (BS.uncons -> raw) (BS.uncons -> paeth) =
	case (priorRaw, prior, raw, paeth) of
		(Nothing, _, _, _) -> error "never occur"
		(_, _, Nothing, _) -> error "never occur"
		(_, Nothing, _, Nothing) -> ""
		(Just (pr, prs), Just (p, ps), Just (r, rs), Just (a, as)) ->
			r' `BS.cons` unfilterPaeth
				(prs `BS.snoc` p) ps (rs `BS.snoc` r') as
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
