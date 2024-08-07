{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BitonicSortPairs where

import Data.Bits
import Data.Bool

bitonicSortPairs :: Integral i => Bool -> i -> i -> [[(i, i)]]
bitonicSortPairs _ _ 0 = []
bitonicSortPairs fl i n =
	zipWith (++)
		(bitonicSortPairs fl i (n - 1))
		(bitonicSortPairs (not fl) (i + 2 ^ (n - 1)) (n - 1)) ++
	bitonicMergePairs fl i n

bitonicMergePairs :: Integral i => Bool -> i -> i -> [[(i, i)]]
bitonicMergePairs _ _ 0 = []
bitonicMergePairs fl i n =
	(bool id flip fl zip)
		[i, i + 1 .. i + 2 ^ (n - 1) - 1]
		[i + 2 ^ (n - 1) , i + 2 ^ (n - 1) + 1 .. i + 2 ^ n - 1] :
	zipWith (++)
		(bitonicMergePairs fl i (n - 1))
		(bitonicMergePairs fl (i + 2 ^ (n - 1)) (n - 1))

getPair :: Int -> Int -> Int -> (Int, Int)
getPair e n i = bitonicSortPairs False 0 e !! n !! i

getPair' :: Int -> Int -> Int -> Int -> (Int, Int)
getPair' e n n' i = bitonicSortPairs False 0 e !! (n * (n + 1) `div` 2 + n') !! i

calcPair :: Int -> Int -> Int -> Int -> (Int, Int)
calcPair e n n' i = bool id (uncurry $ flip (,)) b (i', i' + iv)
	where
	b = (i `shiftR` n) .&. 1 /= 0
	i' = i `div` iv * iv * 2 + i `mod` iv
	iv = 2 ^ (n - n')

{-

	n = 2, n' = 1, i = 3

	i : 00000011
	i': 00000101

	    00000011
	         ^

	n = 2, n' = 1, i = 5

	i : 00000101
	i': 00001001

	    00000101
	         ^

-}

testPairs :: Int -> Int -> Int -> Int -> Bool
testPairs e n n' i = getPair' e n n' i == calcPair e n n' i

separateNum :: Int -> Int -> (Int, Int)
separateNum i n
	| i < n = separateNum (i + 1) (n - i - 1)
	| otherwise = (i, n)

calcPair' :: Int -> Int -> Int -> (Int, Int)
calcPair' e n = uncurry (calcPair e) $ separateNum 0 n
