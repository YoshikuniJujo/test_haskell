module SetT (Set, empty, member, insert, delete) where

import Control.Arrow (second)

data Set a = Empty | Bin {
	_weight :: Int,
	_value :: a,
	left :: (Set a),
	right :: (Set a)
	} deriving Show

empty :: Set a
empty = Empty

weight :: Set a -> Int
weight Empty = 0
weight t = _weight t

bin :: a -> Set a -> Set a -> Set a
bin x l r = Bin (weight l + weight r + 1) x l r

member :: Ord a => a -> Set a -> Bool
member x (Bin _ x0 l r)
	| x < x0 = member x l
	| x > x0 = member x r
	| otherwise = True
member _ _ = False

delta, ratio :: Int
delta = 3
ratio = 2

balance :: Set a -> Set a
balance Empty = Empty
balance t@(Bin _ x l r)
	| weight l + weight r < 2 = t
	| weight r > delta * weight l =
		if weight (left r) >= ratio * weight (right r)
			then rotateL $ bin x l (rotateR r)
			else rotateL t
	| weight l > delta * weight r =
		if weight (right l) >= ratio * weight (left l)
			then rotateR $ bin x (rotateL l) r
			else rotateR t
	| otherwise = t

rotateL, rotateR :: Set a -> Set a
rotateL (Bin _ x lx (Bin _ y ly ry)) = bin y (bin x lx ly) ry
rotateL _ = error "rotateL: can't rotate"
rotateR (Bin _ x (Bin _ y ly ry) rx) = bin y ly (bin x ry rx)
rotateR _ = error "rotateR: can't rotate"

insert :: Ord a => a -> Set a -> Set a
insert x Empty = bin x Empty Empty
insert x t@(Bin _ x0 l r)
	| x < x0 = balance $ bin x0 (insert x l) r
	| x > x0 = balance $ bin x0 l (insert x r)
	| otherwise = t

delete :: Ord a => a -> Set a -> Set a
delete _ Empty = Empty
delete x (Bin _ x0 l r)
	| x < x0 = balance $ bin x0 (delete x l) r
	| x > x0 = balance $ bin x0 l (delete x r)
	| otherwise = glue l r

glue :: Set a -> Set a -> Set a
glue Empty r = r
glue l Empty = l
glue l r
	| weight l > weight r = let (m, l') = popMax l in balance $ bin m l' r
	| otherwise = let (m, r') = popMin r in balance $ bin m l r'

popMin, popMax :: Set a -> (a, Set a)
popMin (Bin _ x Empty r) = (x, r)
popMin (Bin _ x l r) = (balance . flip (bin x) r) `second` popMin l
popMin _ = error "deleteFineMin: Empty"

popMax (Bin _ x l Empty) = (x, l)
popMax (Bin _ x l r) = (balance . bin x l) `second` popMax r
popMax _ = error "popMax: Empty"
