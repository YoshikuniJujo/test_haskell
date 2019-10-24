{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=TypeCheck.Nat #-}

module TypeCheck.Test.FingerTree where

import GHC.TypeLits

import TypeCheck.Test.Range

data FingerTree a
	= Empty
	| Single a
	| Deep (DigitL a) (FingerTree (Node a)) (DigitR a)
	deriving Show

type DigitL = RangeL 1 4
type DigitR = RangeR 1 4
type Node = RangeL 2 3

reducer :: Foldable t => (a -> b -> b) -> t a -> b -> b
reducer = flip . foldr

reducel :: Foldable t => (b -> a -> b) -> b -> t a -> b
reducel = foldl

instance Foldable FingerTree where
	foldr :: forall a b . (a -> b -> b) -> b -> FingerTree a -> b
	foldr _ z Empty = z
	foldr (-<) z (Single x) = x -< z
	foldr (-<) z (Deep pr m sf) = pr -<. (m -<.. (sf -<. z))
		where
		(-<.) :: forall t . Foldable t => t a -> b -> b
		(-<.) = reducer (-<)
		(-<..) :: forall t t' . (Foldable t, Foldable t') => t (t' a) -> b -> b
		(-<..) = reducer (-<.)
	foldl :: forall a b . (b -> a -> b) -> b -> FingerTree a -> b
	foldl _ z Empty = z
	foldl (>-) z (Single x) = z >- x
	foldl (>-) z (Deep pr m sf) = ((z >-. pr) >-.. m) >-. sf
		where
		(>-.) :: forall t . Foldable t => b -> t a -> b
		(>-.) = reducel (>-)
		(>-..) :: forall t t' . (Foldable t, Foldable t') => b -> t (t' a) -> b
		(>-..) = reducel (>-.)

class Nodes m m' where nodes :: RangeL 2 m a -> RangeL 1 m' (Node a)

instance Nodes 3 1 where nodes = (:. NilL)

instance {-# OVERLAPPABLE #-}
	(1 <= ((m' - 1) - 1) + 1, 1 <= (m' - 1) + 1, 1 <= (m' - 1), Nodes (m - 3) (m' - 1)) => Nodes m m' where
	nodes :: forall a . RangeL 2 m a -> RangeL 1 m' (Node a)
	nodes (a :. b :. NilL) = (a :. b :. NilL) :. NilL
	nodes (a :. b :. c :.. NilL) = (a :. b :. c :.. NilL) :. NilL
	nodes (a :. b :. c :.. d :.. NilL) =
		(a :. b :. NilL) :. (c :. d :. NilL) :.. NilL
	nodes (a :. b :. c :.. d :.. e :.. xs) =
		(a :. b :. c :.. NilL) .:..
			(nodes (d :. e :. xs :: RangeL 2 (m - 3) a)
				:: RangeL 1 (m' - 1) (Node a))
	nodes _ = error "never occur"

sample0, sample1, sample2, sample3, sample4 :: RangeL 2 12 Int
sample0	 = 1 :. 2 :. 3 :.. 4 :.. 5 :.. 6 :.. 7 :.. 8 :.. 9 :.. 10 :.. 11 :.. NilL
sample1	 = 1 :. 2 :. 3 :.. 4 :.. 5 :.. 6 :.. 7 :.. 8 :.. 9 :.. 10 :.. NilL
sample2 = 1 :. 2 :. 3 :.. 4 :.. 5 :.. 6 :.. 7 :.. 8 :.. 9 :.. NilL
sample3 = 1 :. 2 :. 3 :.. 4 :.. 5 :.. 6 :.. 7 :.. 8 :.. NilL
sample4 = 1 :. 2 :. 3 :.. 4 :.. 5 :.. 6 :.. 7 :.. NilL
