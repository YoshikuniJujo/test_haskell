module Tree where

data Tree a = Zero a | Succ (Tree (Node a)) deriving Show
data Node a = Node2 a a | Node3 a a a deriving Show
