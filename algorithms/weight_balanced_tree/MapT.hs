module MapT (Map, empty, lookup, insert, delete) where

import Prelude hiding (lookup)

import Control.Arrow (second)

data Map k a = Empty | Bin {
	_weight :: Int,
	_key :: k,
	_value :: a,
	left :: (Map k a),
	right :: (Map k a)
	} deriving Show

empty :: Map k a
empty = Empty

weight :: Map k a -> Int
weight Empty = 0
weight t = _weight t

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r = Bin (weight l + weight r + 1) k x l r

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k (Bin _ k0 x l r)
	| k < k0 = lookup k l
	| k > k0 = lookup k r
	| otherwise = Just x
lookup _ _ = Nothing

delta, ratio :: Int
delta = 3
ratio = 2

balance :: Map k a -> Map k a
balance Empty = Empty
balance t@(Bin _ k x l r)
	| weight l + weight r < 2 = t
	| weight r > delta * weight l =
		if weight (left r) >= ratio * weight (right r)
			then rotateL $ bin k x l (rotateR r)
			else rotateL t
	| weight l > delta * weight r =
		if weight (right l) >= ratio * weight (left l)
			then rotateR $ bin k x (rotateL l) r
			else rotateR t
	| otherwise = t

rotateL, rotateR :: Map k a -> Map k a
rotateL (Bin _ j x lx (Bin _ k y ly ry)) = bin k y (bin j x lx ly) ry
rotateL _ = error "rotateL: can't rotate"
rotateR (Bin _ j x (Bin _ k y ly ry) rx) = bin k y ly (bin j x ry rx)
rotateR _ = error "rotateR: can't rotate"

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k x Empty = bin k x Empty Empty
insert k x t@(Bin _ k0 x0 l r)
	| k < k0 = balance $ bin k0 x0 (insert k x l) r
	| k > k0 = balance $ bin k0 x0 l (insert k x r)
	| otherwise = t

delete :: Ord k => k -> Map k a -> Map k a
delete _ Empty = Empty
delete k (Bin _ k0 x0 l r)
	| k < k0 = balance $ bin k0 x0 (delete k l) r
	| k > k0 = balance $ bin k0 x0 l (delete k r)
	| otherwise = glue l r

glue :: Map k a -> Map k a -> Map k a
glue Empty r = r
glue l Empty = l
glue l r
	| weight l > weight r =
		let ((k, x), l') = popMax l in balance $ bin k x l' r
	| otherwise = let ((k, x), r') = popMin r in balance $ bin k x l r'

popMin, popMax :: Map k a -> ((k, a), Map k a)
popMin (Bin _ k x Empty r) = ((k, x), r)
popMin (Bin _ k x l r) = (balance . flip (bin k x) r) `second` popMin l
popMin _ = error "popMin: Empty"

popMax (Bin _ k x l Empty) = ((k, x), l)
popMax (Bin _ k x l r) = (balance . bin k x l) `second` popMax r
popMax _ = error "popMax: Empty"
