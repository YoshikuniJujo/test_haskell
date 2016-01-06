{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

import Data.Bool

data LR = L | R deriving Show
data LMR = LFT | MID | RGT

data Tree v a = Zero a | Succ (Tree v (Node v a)) deriving Show
data Node v a = Node2 a v a | Node3 a v a v a deriving Show
data Tip a = Tip deriving Show

empty :: Tree v (Tip v)
empty = Zero Tip

class ToList a where
	type Element a :: *
	toListN :: a -> [Element a]
	memberN :: Ord (Element a) => Element a -> a -> Bool
	insertN :: Ord (Element a) => Element a -> a -> Either a (a, Element a, a)

instance ToList (Tip a) where
	type Element (Tip a) = a
	toListN _ = []
	memberN _ _ = False
	insertN a _ = Right (Tip, a, Tip)

instance (ToList a, Element a ~ Element (Node v a)) => ToList (Node v a) where
	type Element (Node v a) = v
	toListN (Node2 l a r) = toListN l ++ [a] ++ toListN r
	toListN (Node3 l a m b r) = toListN l ++ [a] ++ toListN m ++ [b] ++ toListN r
	memberN a (Node2 l b r)
		| a < b = memberN a l
		| a > b = memberN a r
		| otherwise = True
	memberN a (Node3 l b m c r)
		| a == b || a == c = True
		| a < b = memberN a l
		| a > c = memberN a r
		| otherwise = memberN a m
	insertN a t@(Node2 l b r)
		| a < b = case insertN a l of
			Left l' -> Left $ Node2 l' b r
			Right (l', c, m) -> Left $ Node3 l' c m b r
		| a > b = case insertN a r of
			Left r' -> Left $ Node2 l b r'
			Right (m, c, r') -> Left $ Node3 l b m c r'
		| otherwise = Left t
	insertN a t@(Node3 l b m c r)
		| a == b || a == c = Left t
		| a < b = case insertN a l of
			Left l' -> Left $ Node3 l' b m c r
			Right (l', d, m') -> Right (Node2 l' d m', b, Node2 m c r)
		| a < c = case insertN a m of
			Left m' -> Left $ Node3 l b m' c r
			Right (m', d, m'') -> Right (Node2 l b m', d, Node2 m'' c r)
		| otherwise = case insertN a r of
			Left r' -> Left $ Node3 l b m c r'
			Right (m', d, r') -> Right (Node2 l b m, c, Node2 m' d r')

{-
instance (ToList a, Element a ~ Element (Tree v a)) => ToList (Tree v a) where
	type Element (Tree v a) = v
	toListN (Zero a) = toListN a
	toListN (Succ t) = toListN t
	memberN a (Zero n) = memberN a n
	memberN a (Succ t) = memberN a t
	-}

-- data Tree v a = Zero a | Succ (Tree v (Node v a)) deriving Show
-- data Node v a = Node2 v a a | Node3 v v a a a deriving Show


toList :: (ToList a, Element a ~ v) => Tree v a -> [v]
toList (Zero a) = toListN a
toList (Succ t) = toList t

member :: (Ord v, ToList a, Element a ~ v) => v -> Tree v a -> Bool
member a (Zero n) = memberN a n
member a (Succ t) = member a t

insert :: (Ord v, ToList a, Element a ~ v) => v -> Tree v a -> Tree v a
insert a (Zero n) = case insertN a n of
	Left n' -> Zero n'
	Right (l, b, r) -> Succ . Zero $ Node2 l b r
insert a (Succ t) = Succ $ insert a t

-- insert :: Ord a => a -> Tree a (Tip a) -> Tree a (Tip a)
-- insert a (Zero Tip) = Succ . Zero $ Node2 a Tip Tip
