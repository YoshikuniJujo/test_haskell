{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- HALFWAY

module Ch4ASelectionProblem where

smallest :: Ord a => Int -> [a] -> [a] -> a
smallest k xs ys = union xs ys !! k

union :: Ord a => [a] -> [a] -> [a]
union xs [] = xs
union [] ys = ys
union xa@(x : xs) ya@(y : ys)
	| x < y = x : union xs ya
	| x > y = y : union xa ys
union _ _ = error "bad input"
