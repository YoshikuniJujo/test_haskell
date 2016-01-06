{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

import Data.Bool

data LR = L | R deriving Show
data LMR = LFT | MID | RGT

data Tree v a = Zero a | Succ (Tree v (Node v a)) deriving Show
data Node v a = Node2 v a a | Node3 v v a a a deriving Show
data Tip a = Tip deriving Show

empty :: Tree v (Tip v)
empty = Zero Tip

class ToList a where
	type Element a :: *
	type Child a :: *
	type Upper a :: *
	toList :: a -> [Element a]
	member :: Ord (Element a) => Element a -> a -> Bool
	insert :: Ord (Element a) => Element a -> a -> Either a (Upper a)

instance ToList (Tip a) where
	type Element (Tip a) = a
	type Upper (Tip a) = Node a (Tip a)
	toList _ = []
	member _ _ = False
	insert a _ = Right $ Node2 a Tip Tip

instance (ToList a, Element a ~ Element (Node v a)) => ToList (Node v a) where
	type Element (Node v a) = v
	type Upper (Node v a) = Node v (Node v a)
	toList (Node2 a l r) = toList l ++ [a] ++ toList r
	toList (Node3 a b l m r) = toList l ++ [a] ++ toList m ++ [b] ++ toList r
	member a (Node2 b l r)
		| a < b = member a l
		| a > b = member a r
		| otherwise = True
	member a (Node3 b c l m r)
		| a == b || a == c = True
		| a < b = member a l
		| a > c = member a r
		| otherwise = member a m

instance (ToList a, Element a ~ Element (Tree v a)) => ToList (Tree v a) where
	type Element (Tree v a) = v
	type Upper (Tree v a) = Tree v a
	toList (Zero a) = toList a
	toList (Succ t) = toList t
	member a (Zero n) = member a n
	member a (Succ t) = member a t

-- data Tree v a = Zero a | Succ (Tree v (Node v a)) deriving Show
-- data Node v a = Node2 v a a | Node3 v v a a a deriving Show

toListT :: (ToList a, Element a ~ v) => Tree v a -> [v]
toListT (Zero a) = toList a
toListT (Succ t) = toListT t

insertN :: Ord v => a -> Tree v a -> Tree v a
insertN a (Zero b) = undefined
insertN a (Succ t) = undefined

-- insert :: Ord a => a -> Tree a (Tip a) -> Tree a (Tip a)
-- insert a (Zero Tip) = Succ . Zero $ Node2 a Tip Tip
