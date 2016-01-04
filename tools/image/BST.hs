module BST (BST, empty, insert, null, value, left, right) where

import Prelude hiding (null)

data BST a = Bin a (BST a) (BST a) | Tip deriving Show

empty :: BST a
empty = Tip

insert :: Ord a => a -> BST a -> BST a
insert x t@(Bin x0 l r)
	| x < x0 = Bin x0 (insert x l) r
	| x > x0 = Bin x0 l (insert x r)
	| otherwise = t
insert x _ = Bin x Tip Tip

null :: BST a -> Bool
null Tip = True
null _ = False

value :: BST a -> a
value (Bin x _ _) = x

left, right :: BST a -> BST a
left (Bin _ l _) = l
right (Bin _ _ r) = r
