{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Calc (
	calcLength, calcDist,

	fromLiteral',
	fromLength, fromLength',
	fromDist, fromDist'
	) where

import Data.Word
import Data.Bits

calcLength :: Int -> Word16 -> Int
calcLength n eb
	| 257 <= n && n <= 260 = n - 254
	| 261 <= n && n <= 284 = lens !! (n - 261) + fromIntegral eb
	| n == 285 = 258
	| otherwise = error "bad length parameter"

lens :: [Int]
lens = (+ 7) <$> scanl (+) 0 (replicate 4 =<< (2 ^) <$> [0 :: Int ..])

calcDist :: Int -> Word16 -> Int
calcDist n eb
	| 0 <= n && n <= 1 = n + 1
	| 2 <= n && n <= 29 = dists !! (n - 2) + fromIntegral eb
	| otherwise = error "bad distance parameter"

dists :: [Int]
dists = (+ 3) <$> scanl (+) 0 (replicate 2 =<< (2 ^) <$> [0 :: Int ..])

fromLength :: Int -> (Word16, Word8)
fromLength n
	| n < 7 = (fromIntegral n + 254, 0)
	| otherwise = (fromIntegral (length as) + 260, fromIntegral $ n - last as)
	where
	as = takeWhile (<= n) lens

fromLength' :: Int -> Word32
fromLength' n = fromIntegral c `shiftL` 16 .|. fromIntegral e
	where
	(c, e) = fromLength n

fromDist :: Int -> (Word8, Word16)
fromDist n
	| n < 3 = (fromIntegral n - 1, 0)
	| otherwise = (fromIntegral (length as) + 1, fromIntegral $ n - last as)
	where
	as = takeWhile (<= n) dists

fromDist' :: Int -> Word32
fromDist' n = fromIntegral c `shiftL` 16 .|. fromIntegral e
	where
	(c, e) = fromDist n

fromLiteral' :: Int -> Word32
fromLiteral' n = fromIntegral n `shiftL` 16
