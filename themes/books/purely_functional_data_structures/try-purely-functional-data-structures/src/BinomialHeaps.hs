{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BinomialHeaps where

import GHC.Stack (HasCallStack)

data Tree a = Node Int a [Tree a]

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
	| x1 <= x2 = Node (r + 1) x1 (t2 : c1)
	| otherwise = Node (r + 1) x2 (t1 : c2)

type Heap a = [Tree a]

rank :: Tree a -> Int
rank (Node r _ _) = r

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t ta@(t' : ts)
	| rank t < rank t' = t : ta
	| otherwise = insTree (link t t') ts

insert :: Ord a => a -> Heap a -> Heap a
insert x ts = insTree (Node 0 x []) ts

merge :: Ord a => Heap a -> Heap a -> Heap a
merge ts1 [] = ts1
merge [] ts2 = ts2
merge ta1@(t1 : ts1) ta2@(t2 : ts2)
	| rank t1 < rank t2 = t1 : merge ts1 ta2
	| rank t2 < rank t1 = t2 : merge ta1 ts2
	| otherwise = insTree (link t1 t2) $ merge ts1 ts2

root :: Tree a -> a
root (Node _ r _) = r

removeMinTree :: (HasCallStack, Ord a) => Heap a -> (Tree a, Heap a)
removeMinTree [] = error "Empty"
removeMinTree [t] = (t, [])
removeMinTree (t : ts)
	| root t <= root t' = (t, ts)
	| otherwise = (t', t : ts')
	where (t', ts') = removeMinTree ts

findMin :: (HasCallStack, Ord a) => Heap a -> a
findMin = root . fst . removeMinTree

deleteMin :: (HasCallStack, Ord a) => Heap a -> Heap a
deleteMin ts = merge (reverse ts1) ts2
	where (Node _ _ ts1, ts2) = removeMinTree ts
