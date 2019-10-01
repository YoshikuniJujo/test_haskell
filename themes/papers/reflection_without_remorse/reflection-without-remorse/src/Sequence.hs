{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sequence where

import qualified Data.Sequence as Seq

class Sequence s where
	empty :: s a
	singleton :: a -> s a
	(><.) :: s a -> s a -> s a
	viewl :: s a -> ViewL s a

data ViewL s a = EmptyL | a :<. s a deriving Show

instance Sequence Seq.Seq where
	empty = Seq.empty
	singleton = Seq.singleton
	(><.) = (Seq.><)
	viewl s = case Seq.viewl s of
		Seq.EmptyL -> EmptyL
		x Seq.:< xs -> x :<. xs

type CQueue = Seq.Seq

type TreeExp = CQueue Tree

data Tree = Node TreeExp TreeExp | Leaf deriving Show

(<--) :: Tree -> TreeExp -> Tree
Leaf <-- y = val y
Node l r <-- y = Node (l ><. y) (r ><. y)

val :: TreeExp -> Tree
val s = case viewl s of
	EmptyL -> Leaf
	h :<. t -> h <-- t

expr :: Tree -> TreeExp
expr = singleton

(<--.) :: Tree -> Tree -> Tree
l <--. r = l <-- expr r

treeLength :: Tree -> Int
treeLength Leaf = 1
treeLength (Node l r) = treeLength (val l) + treeLength (val r)
