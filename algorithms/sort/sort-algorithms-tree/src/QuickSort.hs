{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module QuickSort where

import Data.Array
import System.Random

import BinaryTree
import Tools

quickSort :: Ord a => StdGen -> [a] -> [a]
quickSort g = destruct . construct (listToBinTree (randoms g :: [Int])) . listToArray

construct :: (Ix i, Integral i, Ord e) => BinTree i -> Array i e -> BinTree e
construct Tip _ = error "not enough pivot indices"
construct (BinTree i_ li ri) a
	| i0 > ix = Tip
	| otherwise = let pvt = a ! i in BinTree pvt
		(construct li . listToArray $ filter (< pvt) es)
		(construct ri . listToArray $ filter (>= pvt) es)
	where
	(i0, ix) = bounds a
	i = i_ `mod` (ix + 1 - i0) + i0
	es = map (a !) $ [i0 .. i - 1] ++ [i + 1 .. ix]

destruct :: BinTree a -> [a]
destruct Tip = []
destruct (BinTree x l r) = destruct l ++ [x] ++ destruct r
