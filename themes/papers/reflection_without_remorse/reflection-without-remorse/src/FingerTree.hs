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

digitToFingerTree :: Digit c a b -> FingerTree c a b
digitToFingerTree (One a) = Single a
digitToFingerTree (Two a b) = Deep (One a) Empty (One b)
digitToFingerTree (Three a b c) = Deep (Two a b) Empty (One c)
digitToFingerTree (Four a b c d) = Deep (Two a b) Empty (Two c d)

data Node c a b
	= forall w . Node2 (c a w) (c w b)
	| forall w x . Node3 (c a w) (c w x) (c x b)

nodeToDigit :: Node c a b -> Digit c a b
nodeToDigit (Node2 a b) = Two a b
nodeToDigit (Node3 a b c) = Three a b c

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

viewr :: FingerTree c a b -> TViewr FingerTree c a b
viewr ft = case ft of
	Empty -> TEmptyR
	Single a -> Empty :| a
	Deep l m (Two a b) -> Deep l m (One a) :| b
	Deep l m (Three a b c) -> Deep l m (Two a b) :| c
	Deep l m (Four a b c d) -> Deep l m (Three a b c) :| d
	Deep l m (One a) -> shiftRight l m :| a
	where
	shiftRight :: Digit c a w -> FingerTree (Node c) w b -> FingerTree c a b
	shiftRight l ft' = case viewr ft' of
		TEmptyR -> digitToFingerTree l
		it :| la -> Deep l it (nodeToDigit la)

infixr 5 :., ++.

data TList c x y where
	Nil :: TList c x x
	(:.) :: c x y -> TList c y z -> TList c x z

(++.) :: TList c a w -> TList c w b -> TList c a b
Nil ++. ys = ys
(x :. xs) ++. ys = x :. (xs ++. ys)

(<||) :: c a w -> FingerTree c w b -> FingerTree c a b
h <|| t = case t of
	Empty -> Single h
	Single a -> Deep (One h) Empty (One a)
	Deep (One a) m r -> Deep (Two h a) m r
	Deep (Two a b) m r -> Deep (Three h a b) m r
	Deep (Three a b c) m r -> Deep (Four h a b c) m r
	Deep (Four a b c d) m r -> Deep (Two h a) (Node3 b c d <|| m) r

pushl :: TList c a w -> FingerTree c w b -> FingerTree c a b
pushl Nil ft = ft
pushl (h :. t) ft = h <|| pushl t ft

(||>) :: FingerTree c a w -> c w b -> FingerTree c a b
it ||> la = case it of
	Empty -> Single la
	Single a -> Deep (One a) Empty (One la)
	Deep l m (One a) -> Deep l m (Two a la)
	Deep l m (Two a b) -> Deep l m (Three a b la)
	Deep l m (Three a b c) -> Deep l m (Four a b c la)
	Deep l m (Four a b c d) -> Deep l (m ||> Node3 a b c) (Two d la)

pushr :: FingerTree c a w -> TList c w b -> FingerTree c a b
pushr ft Nil = ft
pushr ft (h :. t) = pushr (ft ||> h) t

(><) :: FingerTree c a w -> FingerTree c w b -> FingerTree c a b
ft >< ft' = append ft Nil ft'

append :: FingerTree c a w -> TList c w x -> FingerTree c x b -> FingerTree c a b
append Empty xs r = pushl xs r
append (Single a) xs r = a <|| pushl xs r
append l xs Empty = pushr l xs
append l xs (Single a) = pushr l xs ||> a
append (Deep l m r) xs (Deep l' m' r') =
	Deep l (append m (rakeup r xs l') m') r'

rakeup :: Digit c a w -> TList c w x -> Digit c x b -> TList (Node c) a b
rakeup l m r = raiseList $ digitToList l ++. m ++. digitToList r

digitToList :: Digit c a b -> TList c a b
digitToList (One a) = a :. Nil
digitToList (Two a b) = a :. b :. Nil
digitToList (Three a b c) = a :. b :. c :. Nil
digitToList (Four a b c d) = a :. b :. c :. d :. Nil

raiseList :: TList c a b -> TList (Node c) a b
raiseList Nil = Nil
raiseList (_ :. Nil) = error "raseList: can't raise one element"
raiseList (a :. b :. Nil) = Node2 a b :. Nil
raiseList (a :. b :. c :. d :. Nil) = Node2 a b :. Node2 c d :. Nil
raiseList (a :. b :. c :. as) = Node3 a b c :. raiseList as

instance TSequence FingerTree where
	tempty = empty
	tsingleton = singleton
	tviewl = viewl
	tviewr = viewr
	(<|) = (<||)
	(|>) = (||>)
	(|><|) = (><)
