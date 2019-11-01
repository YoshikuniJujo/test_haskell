{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LeftistHeaps where

import GHC.Stack (HasCallStack)
import Control.Exception

class IsHeap h where
	empty :: h a
	isEmpty :: h a -> Bool
	insert :: Ord a => a -> h a -> h a
	merge :: Ord a => h a -> h a -> h a
	findMin :: HasCallStack => h a -> a
	deleteMin :: (HasCallStack, Ord a) => HasCallStack => h a -> h a

data Empty = Empty deriving Show

instance Exception Empty

data Heap a = E | T Int a (Heap a) (Heap a) deriving Show

instance IsHeap Heap where
	empty = E
	isEmpty E = True
	isEmpty (T _ _ _ _) = False
	insert x = merge $ T 1 x E E
	merge = mergeHeap
	findMin E = throw Empty
	findMin (T _ x _ _) = x
	deleteMin E = throw Empty
	deleteMin (T _ _ a b) = merge a b

mergeHeap :: Ord a => Heap a -> Heap a -> Heap a
mergeHeap h E = h
mergeHeap E h = h
mergeHeap h1@(T _ x a1 b1) h2@(T _ y a2 b2)
	| x <= y = makeT x a1 (mergeHeap b1 h2)
	| otherwise = makeT y a2 (mergeHeap h1 b2)

rank :: Heap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b
	| rank a >= rank b = T (rank b + 1) x a b
	| otherwise = T (rank a + 1) x b a

fromList :: Ord a => [a] -> Heap a
fromList = head . mergeAll . map (\x -> T 1 x E E)

mergeAll :: Ord a => [Heap a] -> [Heap a]
mergeAll [] = []
mergeAll [h] = [h]
mergeAll (h1 : h2 : hs) = mergeAll $ merge h1 h2 : mergeAll hs
