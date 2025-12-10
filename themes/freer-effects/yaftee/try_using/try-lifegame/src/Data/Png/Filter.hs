{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png.Filter (filter, unfilter) where

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

-- FILTER

filter :: Int -> [Word8] -> BSF.ByteString -> [Word8]
filter bpp pr raw =
	minimumBy (compare `on` calc) . rights $ filter' bpp pr raw <$> [0 .. 4]

calc :: [Word8] -> Int
calc = sum . ((^ (2 :: Int)) . fromIntegral . fromIntegral @_ @Int8 <$>)

filter' :: Int -> [Word8] -> BSF.ByteString -> Word8 -> Either String [Word8]
filter' bpp pr raw = let zs = BSF.replicate bpp 0 in \case
	0 -> Right $ 0 : otoList raw
	1 -> Right $ 1 : fltrSub zs (otoList raw)
	2 -> Right $ 2 : fltrUp pr raw
	3 -> Right $ 3 : fltrAvr pr zs raw
	4 -> Right $ 4 : fltrPth zs (otoList pr) zs (otoList raw)
	n -> Left $ "unknown filter type: " ++ show n

fltrSub :: BSF.ByteString -> [Word8] -> [Word8]
fltrSub (BSF.uncons -> l) (L.uncons -> c) = case (l, c) of
	(Nothing, _) -> error "never occur"; (_, Nothing) -> []
	(Just (m, ls), Just (c', cs)) -> c' - m : fltrSub (ls `BSF.snoc` c') cs

fltrUp :: [Word8] -> BSF.ByteString -> [Word8]
fltrUp = curry $ uncurry (zipWith subtract) . (id *** otoList)

fltrAvr :: [Word8] -> BSF.ByteString -> BSF.ByteString -> [Word8]
fltrAvr (L.uncons -> u) (BSF.uncons -> l) (BSF.uncons -> c) = case (u, l, c) of
	(_, Nothing, _) -> error "never occur"
	(Nothing, _, Nothing) -> []
	(Just (v, us), Just (m, ls), Just (c', cs)) ->
		c' - avr v m : fltrAvr us (ls `BSF.snoc` c') cs
	_ -> error $ "never occur: fltrAvr: case _: " ++
		show u ++ " " ++ show l ++ " " ++ show c
	where avr x y = fromIntegral $ (toInt x + toInt y) `div` 2

fltrPth :: BSF.ByteString -> [Word8] -> BSF.ByteString -> [Word8] -> [Word8]
fltrPth (BSF.uncons -> ul) (L.uncons -> u) (BSF.uncons -> l) (L.uncons -> c) =
	case (ul, u, l, c) of
		(Nothing, _, _, _) -> error "fltrPth: never occur"
		(_, _, Nothing, _) -> error "fltrPth: never occur"
		(_, Nothing, _, Nothing) -> []
		(Just (vm, uls), Just (v, us), Just (m, ls), Just (c', cs)) ->
			c' - paethPredictor m v vm :
				fltrPth (uls BSF.:> v) us (ls BSF.:> c') cs
		_ -> error "fltrPth: never occur"

-- UNFILTER

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

unfilterSub :: BSF.ByteString -> [Word8] -> [Word8]
unfilterSub (BSF.uncons -> raw) (L.uncons -> sub) = case (raw, sub) of
	(Nothing, _) -> error "never occur"
	(_, Nothing) -> []
	(Just (r, rs), Just (s, ss)) ->
		r' : unfilterSub (rs `BSF.snoc` r') ss
		where r' = r + s

unfilterUp :: [Word8] -> BSF.ByteString -> [Word8]
unfilterUp = curry $ uncurry (zipWith (+)) . (id *** otoList)

unfilterAverage ::
	[Word8] -> BSF.ByteString -> BSF.ByteString -> [Word8]
unfilterAverage (L.uncons -> prior) (BSF.uncons -> raw) (BSF.uncons -> avr) =
	case (prior, raw, avr) of
		(_, Nothing, _) -> error "never occur"
		(Nothing, _, Nothing) -> []
		(Just (p, ps), Just (r, rs), Just (a, as)) ->
			r' : unfilterAverage ps (rs `BSF.snoc` r') as
			where
			r' = fromIntegral ((fromIntegral p + fromIntegral r) `div` 2 :: Int) + a
		_ -> error "never occur"

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

-- PAETH PREDICTOR

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

-- OTHERS

toInt :: Integral n => n -> Int
toInt = fromIntegral
