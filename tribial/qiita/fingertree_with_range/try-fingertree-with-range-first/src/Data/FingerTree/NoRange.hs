{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.FingerTree.NoRange where

data Node a = Node2 a a | Node3 a a a deriving Show
type Digit a = [a]

data FingerTree a
	= Empty | Single a | Deep (Digit a) (FingerTree (Node a)) (Digit a)
	deriving Show

infixr 5 <|

(<|) :: a -> FingerTree a -> FingerTree a
a <| Empty = Single a
a <| Single b = Deep [a] Empty [b]
a <| Deep [b, c, d, e] m sf = Deep [a, b] (Node3 c d e <| m) sf
a <| Deep pr m sf = Deep (a : pr) m sf
