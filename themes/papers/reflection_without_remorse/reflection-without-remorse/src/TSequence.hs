{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TSequence where

class TSequence s where
	tempty :: s c x x
	tsingleton :: c x y -> s c x y
	(|><|) :: s c x y -> s c y z -> s c x z
	tviewl :: s c x y -> TViewl s c x y

	(|>) :: s c x y -> c y z -> s c x z
	(<|) :: c x y -> s c y z -> s c x z

	l |> r = l |><| tsingleton r
	l <| r = tsingleton l |><| r
	l |><| r = case tviewl l of
		TEmptyL -> r
		h :< t -> h <| (t |><| r)

data TViewl s c x y where
	TEmptyL :: TViewl s c x x
	(:<) :: c x y -> s c y z -> TViewl s c x z

{-
newtype TreeCont a b = TreeCont { appTreeCont :: a -> GTree b }

toCont :: GTree a -> TreeCont () a
toCont = TreeCont . const
-}
