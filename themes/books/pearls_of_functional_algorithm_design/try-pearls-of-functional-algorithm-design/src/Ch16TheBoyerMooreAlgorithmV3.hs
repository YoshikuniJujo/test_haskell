{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch16TheBoyerMooreAlgorithmV3 where

import Data.Bool (bool)
import Data.Array

import Ch15AllTheCommonPrefixesV2

matches :: Eq a => [a] -> [a] -> [Int]
matches ws = test m . scanl step (0, [])
	where
	test _ [] = []
	test j ((n, sx) : nxs)
		| i == m = n : test k (drop (k - 1) nxs)
		| m - k <= i = test k (drop (k - 1) nxs)
		| otherwise = test m (drop (k - 1) nxs)
		where
		i' = llcp sw (take j sx)
		i = if i' == j then m else i'
		k = a ! i
	(sw, m) = (reverse ws, length ws)
	step (n, sx) x = (n + 1, x : sx)
	a = accumArray min m (0, m) (vks ++ vks')
		where
		vks = zip (allcp' sw) [1 .. m]
		vks' = zip [m, m - 1 .. 1] (foldr op [] vks)
		op (v, k) ks@(~(k' : _)) = if v + k == m then k : ks else k' : ks

endswith :: Eq a => [a] -> [a] -> Bool
endswith = undefined

llcp :: Eq a => [a] -> [a] -> Int
llcp [] _ = 0; llcp _ [] = 0
llcp (x : xs) (y : ys) = bool 0 (1 + llcp xs ys) (x == y)

allcp' :: Eq a => [a] -> [Int]
allcp' xs = tail (allcp xs) ++ [0]
