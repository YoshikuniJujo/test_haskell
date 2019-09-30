{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PerformanceProblems where

(.++) :: [a] -> [a] -> [a]
[] .++ r = r
(h : t) .++ r = h : t .++ r

data Tree = Node Tree Tree | Leaf deriving Show

(<--) :: Tree -> Tree -> Tree
Leaf <-- y = y
Node l r <-- y = Node (l <-- y) (r <-- y)

treeLength :: Tree -> Int
treeLength Leaf = 1
treeLength (Node Leaf Leaf) = 2
treeLength (Node (Node Leaf Leaf) (Node Leaf Leaf)) = 4
treeLength (Node l r) = treeLength l + treeLength r

data GTree a = GNode (GTree a) (GTree a) | GLeaf a deriving Show

(<---) :: GTree a -> (a -> GTree b) -> GTree b
GLeaf x <--- f = f x
GNode l r <--- f = GNode (l <--- f) (r <--- f)

instance Functor GTree where
	f `fmap` mx = return . f =<< mx

instance Applicative GTree where
	pure = GLeaf
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad GTree where
	(>>=) = (<---)

gTreeLength :: GTree a -> Int
gTreeLength (GLeaf _) = 1
gTreeLength (GNode (GLeaf _) (GLeaf _)) = 2
gTreeLength (GNode (GNode (GLeaf _) (GLeaf _)) (GNode (GLeaf _) (GLeaf _))) = 4
gTreeLength (GNode l r) = gTreeLength l + gTreeLength r
