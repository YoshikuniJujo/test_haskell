{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BinomialHeapsSmart where

data BinomialHeap a = Zero a | Succ (BinomialHeap (Node a)) deriving Show
data Node a = Node a a deriving Show
