{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LazyPairingHeap where

import GHC.Stack (HasCallStack)
import Control.Exception

data Heap a = E | T a !(Heap a) (Heap a) deriving Show

insert :: Ord a => a -> Heap a -> Heap a
insert x a = merge (T x E E) a

findMin :: HasCallStack => Heap a -> a
findMin E = throw Empty
findMin (T x _ _) = x

deleteMin :: (Ord a, HasCallStack) => Heap a -> Heap a
deleteMin E = throw Empty
deleteMin (T _ a b) = merge a b

merge :: Ord a => Heap a -> Heap a -> Heap a
merge a E = a
merge E b = b
merge a@(T x _ _) b@(T y _ _)
	| x <= y = link a b
	| otherwise = link b a

data Empty = Empty deriving Show

instance Exception Empty

link :: (HasCallStack, Ord a) => Heap a -> Heap a -> Heap a
link (T x E m) a = T x a m
link (T x b m) a = T x E $ merge (merge a b) m
link E _ = throw Empty
