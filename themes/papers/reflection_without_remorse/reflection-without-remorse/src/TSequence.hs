{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TSequence where

class TSequence s where
	tempty :: s c x x
	tsingleton :: c x y -> s c x y
	(|><|) :: s c x y -> s c y z -> s c x z
	tviewl :: s c x y -> TViewl s c x y

data TViewl s c x y where
	TEmptyL :: TViewl s c x x
	(:<) :: c x y -> s c y z -> TViewl s c x z

{-
newtype TreeCont a b = TreeCont { appTreeCont :: a -> GTree b }

toCont :: GTree a -> TreeCont () a
toCont = TreeCont . const
-}
