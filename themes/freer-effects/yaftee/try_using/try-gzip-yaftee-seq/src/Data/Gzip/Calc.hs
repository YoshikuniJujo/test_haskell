{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Gzip.Calc (
	calcLength, calcDist,

	fromLiteral',
	fromLength, fromLength',
	fromDist, fromDist',

	lengthToCode, distToCode
	) where

import Data.Word
import Data.Bits

-- import Data.ByteString.Bit qualified as Bit

calcLength :: Int -> Word16 -> Int
calcLength n eb
	| 257 <= n && n <= 284 = lens !! (n - 257) + fromIntegral eb
	| n == 285 = 258
	| otherwise = error "bad length parameter"

lens :: [Int]
lens = (+ 3) <$> scanl (+) 0 ((2 ^) <$> lensBits)

lensBits :: [Int]
lensBits = replicate 4 0 ++ (replicate 4 =<< [0 ..])

calcDist :: Int -> Word16 -> Int
calcDist n eb
	| 0 <= n && n <= 29 = dists !! n + fromIntegral eb
	| otherwise = error "bad distance parameter"

dists :: [Int]
dists = (+ 1) <$> scanl (+) 0 ((2 ^) <$> distsBits)

distsBits :: [Int]
distsBits = replicate 2 0 ++ (replicate 2 =<< [0 ..])

fromLength :: Int -> (Word16, Word8)
fromLength n = (fromIntegral (length as) + 256, fromIntegral $ n - last as)
	where
	as = takeWhile (<= n) lens

fromLength' :: Int -> Word32
fromLength' n = fromIntegral c .|. fromIntegral e `shiftL` 16
	where
	(c, e) = fromLength n

lengthToCode :: Int -> (Int, [Bool])
lengthToCode n = (i + 256, listFromNum el (n - last as))
	where
	i = length as
	as = takeWhile (<= n) lens
	el = lensBits !! (i - 1)

listFromNum :: Bits n => Int -> n -> [Bool]
listFromNum ln n = (n `testBit`) <$> [0 .. ln - 1]

fromDist :: Int -> (Word8, Word16)
fromDist n = (fromIntegral (length as) - 1, fromIntegral $ n - last as)
	where
	as = takeWhile (<= n) dists

fromDist' :: Int -> Word32
fromDist' n = fromIntegral c .|. fromIntegral e `shiftL` 16
	where
	(c, e) = fromDist n

distToCode :: Int -> (Int, [Bool])
distToCode n = (i - 1, listFromNum el (n - last as))
	where
	i = length as
	as = takeWhile (<= n) dists
	el = distsBits !! (i - 1)

fromLiteral' :: Int -> Word32
fromLiteral' n = fromIntegral n
