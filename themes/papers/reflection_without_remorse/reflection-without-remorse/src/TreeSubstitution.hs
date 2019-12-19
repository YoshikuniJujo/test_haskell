{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TreeSubstitution where

data Tree = Node Tree Tree | Tip deriving Show

(<--) :: Tree -> Tree -> Tree
Tip <-- y = y
Node l r <-- y = Node (l <-- y) (r <-- y)

sample1, sample2, sample3 :: Tree
sample1 = Node (Node Tip Tip) (Node Tip Tip)
sample2 = Node Tip (Node Tip Tip)
sample3 = Node (Node Tip Tip) Tip
