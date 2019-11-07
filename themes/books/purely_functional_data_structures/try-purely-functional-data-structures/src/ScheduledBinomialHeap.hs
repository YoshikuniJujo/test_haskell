{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ScheduledBinomialHeap where

data Tree a = Node a [Tree a] deriving Show
data Digit a = Zero | One (Tree a) deriving Show
newtype Schedule a = Schedule { getSchedule :: [[Digit a]] } deriving Show
-- data Heap a = Heap { getHeap :: [Digit a], getSchedule :: Schedule a } deriving Show
data Heap a = Heap [Digit a] (Schedule a) deriving Show

insTree :: Ord a => Tree a -> [Digit a] -> [Digit a]
insTree t [] = [One t]
insTree t (Zero : ds) = One t : ds
insTree t (One t' : ds) = Zero : insTree (link t t') ds

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2)
	| x1 <= x2 = Node x1 (t2 : c1)
	| otherwise = Node x2 (t1 : c2)

exec :: Schedule a -> Schedule a
exec (Schedule []) = Schedule []
exec (Schedule ((One _ : _) : sched)) = Schedule sched
exec (Schedule ((Zero : job) : sched)) = Schedule $ job : sched
exec _ = error "schedule"

insert :: Ord a => a -> Heap a -> Heap a
insert x (Heap ds (Schedule sched)) = Heap ds' (exec . exec . Schedule $ ds' : sched)
	where ds' = insTree (Node x []) ds
