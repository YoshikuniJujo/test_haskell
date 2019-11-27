{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- HALFWAY

module Ch3ImprovingOnSaddlebackSearch where

import Numeric.Natural

type Pair a = (a, a)

type Function = Pair Natural -> Natural

invert :: Function -> Natural -> [Pair Natural]
invert f z = find (0, z) f z

find :: Pair Natural -> Function -> Natural -> [Pair Natural]
find (u, v) f z
	| u > z || v < 0 = []
	| z' < z = find (u + 1, v) f z
	| z' == z = (u, v) : find (u + 1, v - 1) f z
	| z' > z = find (u, v - 1) f z
	| otherwise = error "never occur"
	where z' = f (u, v)

-- bsearch :: (Natural -> Natural) -> Pair Natural -> Natural -> Natural
bsearch :: (Ord t, Integral b) => (b -> t) -> (b, b) -> t -> b
bsearch g (a, b) z
	| a + 1 == b = a
	| g m <= z = bsearch g (m, b) z
	| otherwise = bsearch g (a, m) z
	where m = (a + b) `div` 2

type Function' = Pair Integer -> Integer

invert' :: Function' -> Integer	-> [Pair Integer]
invert' f z = find' (0, m) f z n
	where
	m = bsearch (\y -> f (0, y)) (- 1, z + 1) z
	n = bsearch (\x -> f (x, 0)) (- 1, z + 1) z

find' :: Pair Integer -> Function' -> Integer -> Integer -> [Pair Integer]
find' (u, v) f z n
	| u > n || v < 0 = []
	| z' < z = find' (u + 1, v) f z n
	| z' == z = (u, v) : find' (u + 1, v - 1) f z n
	| z' > z = find' (u, v - 1) f z n
	| otherwise = error "never occur"
	where z' = f (u, v)
