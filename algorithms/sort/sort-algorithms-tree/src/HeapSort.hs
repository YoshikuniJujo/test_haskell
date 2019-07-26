{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HeapSort where

import Data.List
import Data.Array

import BinaryTree
import Tools

heapSort :: Ord a => [a] -> [a]
heapSort = heapSortArray . (listToArray :: [a] -> Array Int a)

heapSortArray :: (Ix i, Integral i, Ord e) => Array i e -> [e]
heapSortArray = destruct . (`construct` 0)

construct :: (Ix i, Integral i, Ord e) => Array i e -> i -> BinTree e
construct a i
	| li > ix = BinTree (a ! i) Tip Tip
	| ri > ix = heapTree (a ! i) (construct a li) Tip
	| otherwise = heapTree (a ! i) (construct a li) (construct a ri)
	where
	(i0, ix) = bounds a
	li = i * 2 - i0 + 1
	ri = i * 2 - i0 + 2

heapTree :: Ord a => a -> BinTree a -> BinTree a -> BinTree a
heapTree p l@(BinTree lv ll lr) r@(BinTree rv rl rr)
	| p <= lv && p <= rv = BinTree p l r
	| lv <= rv = BinTree lv (heapTree p ll lr) r
	| otherwise = BinTree rv l (heapTree p rl rr)
heapTree p l@(BinTree lv ll lr) Tip
	| p <= lv = BinTree p l Tip
	| otherwise = BinTree lv (heapTree p ll lr) Tip
heapTree p Tip r@(BinTree rv rl rr)
	| p <= rv = BinTree p Tip r
	| otherwise = BinTree rv Tip (heapTree p rl rr)
heapTree p Tip Tip = BinTree p Tip Tip

destruct :: Ord a => BinTree a -> [a]
destruct = unfoldr $ \case Tip -> Nothing; t -> Just $ popTree t

popTree :: Ord a => BinTree a -> (a, BinTree a)
popTree (BinTree x l@(BinTree lv _ _)  r@(BinTree rv _ _))
	| lv <= rv = let (_, l') = popTree l in (x, BinTree lv l' r)
	| otherwise = let (_, r') = popTree r in (x, BinTree rv l r')
popTree (BinTree x l@(BinTree lv _ _) Tip) = let (_, l') = popTree l in (x, BinTree lv l' Tip)
popTree (BinTree x Tip r@(BinTree rv _ _)) = let (_, r') = popTree r in (x, BinTree rv Tip r')
popTree (BinTree x Tip Tip) = (x, Tip)
popTree Tip = error "Oops!"
