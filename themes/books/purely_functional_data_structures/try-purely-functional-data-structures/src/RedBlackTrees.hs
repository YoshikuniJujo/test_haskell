{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RedBlackTrees where

data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T _ a y b)
	| x < y = member x a
	| x > y = member x b
	| otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert x s = T B a' y' b'
	where
	ins E = T R E x E
	ins s'@(T c a y b)
		| x < y = balance c (ins a) y b
		| x > y = balance c a y (ins b)
		| otherwise = s'
	T _ a' y' b' = ins s

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B t1 x0 t2 = case (t1, x0, t2) of
	(T R (T R a x b) y c, z, d) -> f a x b y c z d
	(T R a x (T R b y c), z, d) -> f a x b y c z d
	(a, x, T R (T R b y c) z d) -> f a x b y c z d
	(a, x, T R b y (T R c z d)) -> f a x b y c z d
	_ -> T B t1 x0 t2
	where
	f a x b y c z d = T R (T B a x b) y (T B c z d)
balance c t1 x0 t2 = T c t1 x0 t2

{-
mergeOrdTrees :: Tree a -> Tree a -> Tree a
mergeOrdTrees E t = t
mergeOrdTrees t E = t
mergeOrdTrees (T _ a x b) (T _ c y d) = balance B a x (T R c y d)
-}

-- fromOrdList :: [a] -> Tree a
-- fromOrdList = 

-- fromOrdListGen :: Tree a -> [(a, Tree a)] -> (Tree a, [(a, Tree a)])
-- fromOrdListGen a ((x, b) : ts) = (T B a x b, 
--	where (t, ts') = fromOrdListGen ts
