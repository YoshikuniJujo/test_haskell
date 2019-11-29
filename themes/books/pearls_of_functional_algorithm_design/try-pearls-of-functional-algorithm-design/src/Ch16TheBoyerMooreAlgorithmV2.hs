{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch16TheBoyerMooreAlgorithmV2 where

import Data.Bool (bool)

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
		k = shift i
	(sw, m) = (reverse ws, length ws)
	step (n, sx) x = (n + 1, x : sx)
	shift i = head [ k | k <- [1 .. m], llcp sw (drop k sw) == min i (m - k)]

endswith :: Eq a => [a] -> [a] -> Bool
endswith = undefined

llcp :: Eq a => [a] -> [a] -> Int
llcp [] _ = 0; llcp _ [] = 0
llcp (x : xs) (y : ys) = bool 0 (1 + llcp xs ys) (x == y)
