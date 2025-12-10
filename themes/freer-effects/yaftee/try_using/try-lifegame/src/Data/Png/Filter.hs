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
		c' - average v m : fltrAvr us (ls `BSF.snoc` c') cs
	_ -> error $ "never occur: fltrAvr: case _: " ++
		show u ++ " " ++ show l ++ " " ++ show c

average :: Integral n => n -> n -> n
average x y = fromIntegral $ (toInt x + toInt y) `div` 2

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
unfilter bpp pr (BSF.uncons -> fltrd) = case fltrd of
	Nothing -> Left "empty line"
	Just (ft, fs) -> case ft of
		0 -> Right $ otoList fs
		1 -> Right $ unfltrSub zs (otoList fs)
		2 -> Right $ unfltrUp pr fs
		3 -> Right $ unfltrAvr pr zs fs
		4 -> Right $ unfltrPth zs (otoList pr) zs (otoList fs)
		_ -> Left $ "unknown filter type: " ++ show ft ++ " " ++ show fs
	where zs = BSF.replicate bpp 0

unfltrSub :: BSF.ByteString -> [Word8] -> [Word8]
unfltrSub (BSF.uncons -> l) (L.uncons -> sub) = case (l, sub) of
	(Nothing, _) -> error "never occur"; (_, Nothing) -> []
	(Just (m, ls), Just (s, ss)) -> let c = m + s in
		c : unfltrSub (ls `BSF.snoc` c) ss

unfltrUp :: [Word8] -> BSF.ByteString -> [Word8]
unfltrUp = curry $ uncurry (zipWith (+)) . (id *** otoList)

unfltrAvr :: [Word8] -> BSF.ByteString -> BSF.ByteString -> [Word8]
unfltrAvr (L.uncons -> u) (BSF.uncons -> l) (BSF.uncons -> avr) =
	case (u, l, avr) of
		(_, Nothing, _) -> error "never occur";
		(Nothing, _, Nothing) -> []
		(Just (v, us), Just (m, ls), Just (a, as)) ->
			c : unfltrAvr us (ls `BSF.snoc` c) as
			where c = average v m + a
		_ -> error "never occur"

unfltrPth :: BSF.ByteString -> [Word8] -> BSF.ByteString -> [Word8] -> [Word8]
unfltrPth (BSF.uncons -> ul) (L.uncons -> u) (BSF.uncons -> l) (L.uncons -> p) =
	case (ul, u, l, p) of
		(Nothing, _, _, _) -> error "never occur"
		(_, _, Nothing, _) -> error "never occur"
		(_, Nothing, _, Nothing) -> []
		(Just (vm, uls), Just (v, us), Just (m, ls), Just (p', ps)) ->
			c : unfltrPth (uls BSF.:> v) us (ls BSF.:> c) ps
			where c = p' + paethPredictor m v vm
		_ -> error "never occur"

-- PAETH PREDICTOR

paethPredictor :: Word8 -> Word8 -> Word8 -> Word8
paethPredictor (id &&& fromIntegral -> (l, l'))
	(id &&& fromIntegral -> (u, u')) (id &&& fromIntegral -> (ul, ul'))
	| pl <= pu && pl <= pul = l
	| pu <= pul = u
	| otherwise = ul
	where
	p = l' + u' - ul' :: Int
	pl = abs $ p - l'; pu = abs $ p - u'; pul = abs $ p - ul'

-- OTHERS

toInt :: Integral n => n -> Int
toInt = fromIntegral
