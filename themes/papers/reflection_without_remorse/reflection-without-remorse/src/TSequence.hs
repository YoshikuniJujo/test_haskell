{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TSequence where

class TSequence s where
	tempty :: s c x x
	tsingleton :: c x y -> s c x y
	(|><|) :: s c x y -> s c y z -> s c x z
	tviewl :: s c x y -> TViewl s c x y
	tviewr :: s c x y -> TViewr s c x y

	(|>) :: s c x y -> c y z -> s c x z
	(<|) :: c x y -> s c y z -> s c x z

	l |> r = l |><| tsingleton r
	l <| r = tsingleton l |><| r
	l |><| r = case tviewl l of
		TEmptyL -> r
		h :< t -> h <| (t |><| r)

	tviewl q = case tviewr q of
		TEmptyR -> TEmptyL
		p :| l -> case tviewl p of
			TEmptyL -> l :< tempty
			h :< t -> h :< (t |> l)

	tviewr q = case tviewl q of
		TEmptyL -> TEmptyR
		h :< t -> case tviewr t of
			TEmptyR -> tempty :| h
			p :| l -> (h <| p) :| l

data TViewl s c x y where
	TEmptyL :: TViewl s c x x
	(:<) :: c x y -> s c y z -> TViewl s c x z

data TViewr s c x y where
	TEmptyR :: TViewr s c x x
	(:|) :: s c x y -> c y z -> TViewr s c x z

{-
newtype TreeCont a b = TreeCont { appTreeCont :: a -> GTree b }

toCont :: GTree a -> TreeCont () a
toCont = TreeCont . const
-}
