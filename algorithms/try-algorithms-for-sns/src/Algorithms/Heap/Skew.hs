{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Algorithms.Heap.Skew (
	Heap, empty, singleton, fromList, toList,
	insert, deleteMin, findMin, isEmpty ) where

import Data.List qualified as L

data Heap a = Tip | Heap a (Heap a) (Heap a) deriving Show

empty :: Heap a; empty = Tip
singleton :: a -> Heap a; singleton x = Heap x Tip Tip
isEmpty :: Heap a -> Bool; isEmpty = \case Tip -> True; Heap _ _ _ -> False
insert :: Ord a => Heap a -> a -> Heap a; insert h = merge h . singleton

deleteMin :: Ord a => Heap a -> Maybe (a, Heap a)
deleteMin = \case Tip -> Nothing; Heap x l r -> Just (x, l `merge` r)

findMin :: Heap a -> Maybe a
findMin = \case Tip -> Nothing; Heap x _ _ -> Just x

fromList :: Ord a => [a] -> Heap a; fromList = foldl insert empty
toList :: Ord a => Heap a -> [a]; toList = L.unfoldr deleteMin

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Tip = h; merge Tip h = h
merge h1@(Heap x l1 r1) h2@(Heap y l2 r2)
	| x < y = Heap x (merge r1 h2) l1
	| otherwise = Heap y (merge r2 h1) l2
