{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.List
import Numeric.Natural

toDec :: Natural -> [Word]
toDec 0 = []
toDec n = fromIntegral (n `mod` 10) : toDec (n `div` 10)

fromDec :: [Word] -> Natural
fromDec [] = 0
fromDec (d : ds) = fromIntegral d + 10 * fromDec ds

maxNum, minNum :: Natural -> Natural
maxNum = fromDec . sortBy compare . toDec
minNum = fromDec . sortBy (flip compare) . toDec

step :: Natural -> Natural
step = subtract <$> minNum <*> maxNum

untilFix :: Eq a => (a -> a) -> a -> a
untilFix f x | x == x' = x | otherwise = untilFix f x' where x' = f x

untilFixScan :: Eq a => (a -> a) -> a -> [a]
untilFixScan f x | x == x' = [x] | otherwise = x : untilFixScan f x'
	where x' = f x
