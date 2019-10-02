{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FingerTree where

import TSequence

data FingerTree c a b where
	Empty :: FingerTree c x x
	Single :: c a b -> FingerTree c a b
	Deep :: Digit c a w ->
		FingerTree (Node c) w x -> Digit c x b -> FingerTree c a b

{-
data FingerTree c a b
	= Empty
	| Single (c a b)
	| forall w x . Deep (Digit c a w) (FingerTree (Node c) w x) (Digit c x b)
	-}

data Digit c a b
 	= One (c a b)
	| forall w . Two (c a w) (c w b)
	| forall w x . Three (c a w) (c w x) (c x b)
	| forall w x y . Four (c a w) (c w x) (c x y) (c y b)

data Node c a b
	= forall w . Node2 (c a w) (c w b)
	| forall w x . Node3 (c a w) (c w x) (c x b)

empty :: FingerTree c x x
empty = Empty

singleton :: c a b -> FingerTree c a b
singleton = Single

viewl :: FingerTree c a b -> TViewl FingerTree c a b
viewl ft = case ft of
	Empty -> TEmptyL
	Single a -> a :< Empty
	Deep (Two a b) m r -> a :< Deep (One b) m r
	Deep (Three a b c) m r -> a :< Deep (Two b c) m r
	Deep (Four a b c d) m r -> a :< Deep (Three b c d) m r
	Deep (One a) m r -> a :< shiftLeft m r
	where
	shiftLeft :: FingerTree (Node c) a w -> Digit c w b -> FingerTree c a b
	shiftLeft ft' r = case viewl ft' of
		TEmptyL -> digitToFingerTree r
		h :< t -> Deep (nodeToDigit h) t r
	digitToFingerTree (One a) = Single a
	digitToFingerTree (Two a b) = Deep (One a) Empty (One b)
	digitToFingerTree (Three a b c) = Deep (Two a b) Empty (One c)
	digitToFingerTree (Four a b c d) = Deep (Two a b) Empty (Two c d)
	nodeToDigit (Node2 a b) = Two a b
	nodeToDigit (Node3 a b c) = Three a b c
