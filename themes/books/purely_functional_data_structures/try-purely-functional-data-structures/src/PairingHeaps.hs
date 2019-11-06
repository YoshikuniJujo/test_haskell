{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PairingHeaps where

import GHC.Stack (HasCallStack)
import Control.Exception

data Heap a = E | T a [Heap a] deriving Show

data Empty = Empty deriving Show

instance Exception Empty

findMin :: HasCallStack => Heap a -> a
findMin E = throw Empty
findMin (T x _) = x

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T x hs1) h2@(T y hs2)
	| x <= y = T x (h2 : hs1)
	| otherwise = T y (h1 : hs2)

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (T x []) h

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1 : h2 : hs) = merge (merge h1 h2) (mergePairs hs)

deleteMin :: (HasCallStack, Ord a) => Heap a -> Heap a
deleteMin E = throw Empty
deleteMin (T _ hs) = mergePairs hs
