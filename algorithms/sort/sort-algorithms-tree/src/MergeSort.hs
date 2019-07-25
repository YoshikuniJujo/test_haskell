{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MergeSort where

import Data.Array

import BinaryTree
import Tools

mergeSort :: forall a . Ord a => [a] -> [a]
mergeSort = destruct . construct . (listToArray :: [a] -> Array Int a)

construct :: (Ix i, Integral i) => Array i e -> BinTree e
construct a
	| i0 > ix = Tip
	| otherwise = BinTree (a ! md)
		(construct $ ixmap (i0, md - 1) id a)
		(construct $ ixmap (md + 1, ix) id a)
	where
	(i0, ix) = bounds a
	md = (i0 + ix) `div` 2

destruct :: Ord a => BinTree a -> [a]
destruct Tip = []
destruct (BinTree x l r) = merge3 [x] (destruct l) (destruct r)

merge3 :: Ord a => [a] -> [a] -> [a] -> [a]
merge3 [] ys zs = merge ys zs
merge3 xs [] zs = merge xs zs
merge3 xs ys [] = merge xs ys
merge3 xa@(x : xs) ya@(y : ys) za@(z : zs)
	| x <= y && x <= z = x : merge3 xs ya za
	| y <= z = y : merge3 xa ys za
	| otherwise = z : merge3 xa ya zs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xa@(x : xs) ya@(y : ys)
	| x <= y = x : merge xs ya
	| otherwise = y : merge xa ys
