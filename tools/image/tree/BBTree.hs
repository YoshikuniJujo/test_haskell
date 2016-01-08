{-# LANGUAGE TypeFamilies #-}

module BBTree (
	Tree, Tip,
	BBTree.null, height1, decompose, value, children, sample) where

data Tree v a = Zero a | Succ (Tree v (Node v a)) deriving Show
data Node v a = Node2 a v a | Node3 a v a v a deriving Show
data Tip v = Tip deriving Show

null :: Tree v a -> Bool
null (Zero _) = True
null _ = False

height1 :: Tree v a -> Bool
height1 (Succ (Zero _)) = True
height1 _ = False

sample :: Tree Int (Tip Int)
sample = Succ . Succ . Succ . Succ . Zero $ Node2
	(Node2
		(Node3
			(Node2 Tip 0 Tip)
			1
			(Node3 Tip 2 Tip 3 Tip)
			4
			(Node2 Tip 5 Tip))
		6
		(Node2
			(Node2 Tip 7 Tip)
			8
			(Node3 Tip 9 Tip 10 Tip)))
	11
	(Node3
		(Node3
			(Node2 Tip 12 Tip)
			13
			(Node2 Tip 14 Tip)
			15
			(Node2 Tip 16 Tip))
		17
		(Node2
			(Node2 Tip 18 Tip)
			19
			(Node3 Tip 20 Tip 21 Tip))
		22
		(Node2
			(Node2 Tip 23 Tip)
			24
			(Node2 Tip 25 Tip)))

class IsNode a where
	type Elem a
	decomposeN :: Node (Elem a) a ->
		Either (a, Elem a, a) (a, Elem a, a, Elem a, a)
	valueN :: Node (Elem a) a -> Either (Elem a) (Elem a, Elem a)
	childrenN :: Node (Elem a) a -> Either (a, a) (a, a, a)

instance IsNode (Tip v) where
	type Elem (Tip v) = v
	decomposeN (Node2 _ d _) = Left (Tip, d, Tip)
	decomposeN (Node3 _ c _ f _) = Right (Tip, c, Tip, f, Tip)
	valueN (Node2 _ d _) = Left d
	valueN (Node3 _ c _ f _) = Right (c, f)
	childrenN (Node2 _ _ _) = Left (Tip, Tip)
	childrenN (Node3 _ _ _ _ _) = Right (Tip, Tip, Tip)

instance IsNode (Node v a) where
	type Elem (Node v a) = v
	decomposeN (Node2 l d r) = Left (l, d, r)
	decomposeN (Node3 l c o f r) = Right (l, c, o, f, r)
	valueN (Node2 _ d _) = Left d
	valueN (Node3 _ c _ f _) = Right (c, f)
	childrenN (Node2 l _ r) = Left (l, r)
	childrenN (Node3 l _ o _ r) = Right (l, o, r)

decompose :: (IsNode a, v ~Elem a) => Tree v a ->
	Either (Tree v a, v, Tree v a) (Tree v a, v, Tree v a, v, Tree v a)
decompose (Succ (Zero n)) = case decomposeN n of
	Left (l, d, r) -> Left (Zero l, d, Zero r)
	Right (l, c, o, f, r) -> Right (Zero l, c, Zero o, f, Zero r)
decompose (Succ t) = case decompose t of
	Left (l, d, r) -> Left (Succ l, d, Succ r)
	Right (l, c, o, f, r) -> Right (Succ l, c, Succ o, f, Succ r)

value :: (IsNode a, v ~ Elem a) => Tree v a -> Either v (v, v)
value (Succ (Zero n)) = valueN n
value (Succ t) = value t

children :: (IsNode a, v ~ Elem a) =>
	Tree v a -> Either (Tree v a, Tree v a) (Tree v a, Tree v a, Tree v a)
children (Succ (Zero n)) = case childrenN n of
	Left (l, r) -> Left (Zero l, Zero r)
	Right (l, o, r) -> Right (Zero l, Zero o, Zero r)
children (Succ t) = case children t of
	Left (l, r) -> Left (Succ l, Succ r)
	Right (l, o, r) -> Right (Succ l, Succ o, Succ r)
