{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SplayHeaps where

import GHC.Stack (HasCallStack)
import Control.Exception

data Tree a = E | T (Tree a) a (Tree a) deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert x t = T (smaller x t) x (bigger x t)

bigger :: Ord a => a -> Tree a -> Tree a
bigger _ E = E
bigger p (T a x b)
	| x <= p = bigger p b
	| otherwise = case a of
		E -> T E x b
		T a1 y a2
			| y <= p -> T (bigger p a2) x b
			| otherwise -> T (bigger p a1) y (T a2 x b)

smaller :: Ord a => a -> Tree a -> Tree a
smaller _ E = E
smaller p (T a x b)
	| x > p = smaller p a
	| otherwise = case b of
		E -> T a x E
		T b1 y b2
			| y > p -> T a x (smaller p b1)
			| otherwise -> T (T a x b1) y (smaller p b2)

partition :: Ord a => a -> Tree a -> (Tree a, Tree a)
partition _ E = (E, E)
partition p t@(T a x b)
	| x <= p = case b of
		E -> (t, E)
		T b1 y b2
			| y <= p -> let (small, big) = partition p b2 in
				(T (T a x b1) y small, big)
			| otherwise -> let (small, big) = partition p b1 in
				(T a x small, T big y b2)
	| otherwise = case a of
		E -> (E, t)
		T a1 y a2
			| y <= p -> let (small, big) = partition p a2 in
				(T a1 y small, T big x b)
			| otherwise -> let (small, big) = partition p a1 in
				(small, T big y (T a2 x b))

data Empty = Empty deriving Show

instance Exception Empty

findMin :: HasCallStack => Tree a -> a
findMin E = throw Empty
findMin (T E x _) = x
findMin (T a _ _) = findMin a

deleteMin :: HasCallStack => Tree a -> Tree a
deleteMin E = throw Empty
deleteMin (T E _ b) = b
deleteMin (T (T E _ b) y c) = T b y c
deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)
