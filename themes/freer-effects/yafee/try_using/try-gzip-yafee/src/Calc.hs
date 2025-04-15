{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Calc (calcLength, calcDist) where

import Data.Word

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
