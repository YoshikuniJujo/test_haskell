{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DfsBfs where

data Tree a = Tip | Node (Tree a) a (Tree a) deriving Show

tree :: Tree Int
tree = Node
	(Node
		(Node (Node Tip 789 Tip) 987 Tip)
		888
		(Node Tip 456 Tip))
	999
	(Node
		(Node Tip 123 Tip)
		777
		Tip)

dfs :: Tree a -> [a]
dfs Tip = []
dfs (Node l x r) = dfs l ++ [x] ++ dfs r

bfs :: [Tree a] -> [a]
bfs [] = []
bfs (Tip : ts) = bfs ts
bfs (Node l x r : ts) = x : bfs (ts ++ [l, r])

bfs' :: Tree a -> [a]
bfs' t = postprocess queue
	where
	queue = t : walk 1 queue

walk :: Int -> [Tree a] -> [Tree a]
walk 0 _ = []
walk n (Tip : q) = walk (n - 1) q
walk n (Node l _ r : q) = l : r : walk (n + 1) q
walk _ _ = error "walk"

postprocess :: [Tree a] -> [a]
postprocess = map extract . filter isNode

extract :: Tree a -> a
extract (Node _ x _) = x
extract _ = error "extract"

isNode :: Tree t -> Bool
isNode (Node _ _ _) = True
isNode _ = False
