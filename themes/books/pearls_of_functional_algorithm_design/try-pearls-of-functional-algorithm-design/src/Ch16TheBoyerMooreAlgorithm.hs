{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch16TheBoyerMooreAlgorithm where

import Data.Bool (bool)
-- import Data.List (inits)

matches :: Eq a => [a] -> [a] -> [Int]
-- matches ws = map length . filter (endswith ws) . inits
matches ws = test . scanl step (0, [])
	where
	test [] = []
	test ((n, sx) : nxs) = bool id (n :) (i == m) $ test (drop (k - 1) nxs)
		where
		i = llcp sw sx
		k = shift i
	(sw, m) = (reverse ws, length ws)
	step (n, sx) x = (n + 1, x : sx)
	shift i = head [ k | k <- [1 .. m], llcp sw (drop k sw) == min i (m - k)]

endswith :: Eq a => [a] -> [a] -> Bool
endswith = undefined

llcp :: Eq a => [a] -> [a] -> Int
llcp [] _ = 0; llcp _ [] = 0
llcp (x : xs) (y : ys) = bool 0 (1 + llcp xs ys) (x == y)
