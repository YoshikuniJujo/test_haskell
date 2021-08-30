{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Arrow
import Data.List

popFact :: Int -> Int -> (Int, Int)
popFact n k
	| k ^ (2 :: Int) > n = (n, 1)
	| otherwise = case n `mod` k of
		0 -> (k, n `div` k)
		_ -> popFact n $ k + 1

facts :: Int -> [(Int, Int)]
facts = map (head &&& length) . group . unfoldr \case 1 -> Nothing; n -> Just $ popFact n 2

factsNum :: Int -> Int
factsNum = product . map ((+ 1) . snd) . facts

factsNums :: [Int]
factsNums = 0 : map factsNum [1 ..]

primes :: [Int]
primes = findIndices (== 2) factsNums

hcns :: [Int]
hcns = takeHcns 0 0 factsNums

takeHcns :: Int -> Int -> [Int] -> [Int]
takeHcns _ _ [] = []
takeHcns i mx (n : ns)
	| n > mx = i : takeHcns (i + 1) n ns
	| otherwise = takeHcns (i + 1) mx ns
