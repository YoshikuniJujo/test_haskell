{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Test.TFingerTree where

{-
data TFingerTree :: (* -> * -> *) -> * -> * -> * where
	TEmpty :: TFingerTree c x x
	TSingle :: c x y -> TFingerTree c x y
	TDeep :: TDigitL c x y ->
		TFingerTree (TNode c) y z -> TDigitR c z w -> TFingerTree c x w

type TDigitL = TRange
-}
