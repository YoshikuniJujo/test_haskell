{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch1TheSmallestFreeNumber where

import Data.List
import Data.Array

minfree :: [Int] -> Int
-- minfree xs = head ([0 ..] \\ xs)
minfree xs = minfrom 0 (length xs, xs)

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n)
	$ zip (filter (<= n) xs) (repeat True)
	where n = length xs

minfrom :: Int -> (Int, [Int]) -> Int
-- minfrom a xs = head ([a ..] \\ xs)
minfrom a (n, xs)
	| n == 0 = a
	| m == b - a = minfrom b (m, vs)
	| otherwise = minfrom a (m, us)
	where
	(us, vs) = partition (< b) xs
	b = a + 1 + n `div` 2
	m = length us
