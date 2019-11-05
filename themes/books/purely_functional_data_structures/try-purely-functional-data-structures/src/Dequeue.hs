{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Dequeue where

import Prelude hiding (head, tail, last, init)

import GHC.Stack(HasCallStack)
import Control.Arrow
import Control.Exception

class Dequeue q where
	empty :: q a
	isEmpty :: q a -> Bool

	cons :: a -> q a -> q a
	head :: HasCallStack => q a -> a
	tail :: HasCallStack => q a -> q a

	snoc :: q a -> a -> q a
	last :: HasCallStack => q a -> a
	init :: HasCallStack => q a -> q a

data Empty = Empty deriving Show

instance Exception Empty

bad :: a
bad = throw Empty

half :: [a] -> ([a], [a])
half lst = hf lst lst
	where
	hf [] xs = ([], xs)
	hf [_] xs = ([], xs)
	hf (_ : _ : cs) (x : xs) = (x :) `first` hf cs xs
	hf _ _ = error "never occur"

data DQ a = DQ [a] [a] deriving Show

checkf :: DQ a -> DQ a
checkf dq@(DQ [] []) = dq
checkf dq@(DQ [] [_]) = dq
checkf dq@(DQ [_] []) = dq
checkf (DQ [] r) = DQ f' r'
	where (r', f') = reverse `second` half r
checkf (DQ f []) = DQ f' r'
	where (f', r') = reverse `second` half f
checkf dq = dq

instance Dequeue DQ where
	empty = DQ [] []
	isEmpty (DQ [] []) = True
	isEmpty _ = False

	cons x (DQ f r) = checkf $ DQ (x : f) r
	head (DQ (x : _) _) = x
	head (DQ [] _) = bad
	tail (DQ (_ : f) r) = checkf $ DQ f r
	tail (DQ [] [_]) = empty
	tail _ = bad

	snoc (DQ f r) x = checkf $ DQ f (x : r)
	last (DQ _ (x : _)) = x
	last (DQ _ []) = bad
	init (DQ f (_ : r)) = checkf $ DQ f r
	init (DQ [_] []) = empty
	init _ = bad
