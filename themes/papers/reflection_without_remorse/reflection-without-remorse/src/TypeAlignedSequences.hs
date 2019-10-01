{-# LANGUAGE GADTs, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module TypeAlignedSequences where

import Prelude hiding ((.))
import Control.Category (Category(..))

import Sequence
import TSequence

infixr 5 :.

data TList c x y where
	Nil :: TList c x x
	(:.) :: c x y -> TList c y z -> TList c x z

data AsUnitLoop a b c where UL :: a -> AsUnitLoop a () ()

x :: TList (AsUnitLoop Integer) () ()
x = UL 123 :. UL 345 :. UL 567 :. Nil

newtype AsSequence s a = AsSequence { getAsSequence :: s (AsUnitLoop a) () () }

instance TSequence s => Sequence (AsSequence s) where
	empty = AsSequence tempty
	singleton = AsSequence . tsingleton . UL
	AsSequence a ><. AsSequence b = AsSequence $ a |><| b
	viewl (AsSequence s) = case tviewl s of
		TEmptyL -> EmptyL
		UL h :< t -> h :<. AsSequence t

instance Sequence s => Semigroup (s a) where
	(<>) = (><.)

instance Sequence s => Monoid (s a) where
	mempty = empty

instance TSequence s => Category (s c) where
	id = tempty
	(.) = flip (|><|)

data Pair c a b where
	(:*) :: c a w -> c w b -> Pair c a b

data Buffer c a b where
	B1 :: c a b -> Buffer c a b
	B2 :: Pair c a b -> Buffer c a b

data Queue c a b where
	Q0 :: Queue c a a
	Q1 :: c a b -> Queue c a b
	QN :: Buffer c a x -> Queue (Pair c) x y -> Buffer c y b -> Queue c a b

(||>) :: Queue c a w -> c w b -> Queue c a b
q ||> b = case q of
	Q0 -> Q1 b
	Q1 a -> QN (B1 a) Q0 (B1 b)
	QN l m (B1 a) -> QN l m (B2 (a :* b))
	QN l m (B2 r) -> QN l (m ||> r) (B1 b)

viewl' :: Queue c a b -> TViewl Queue c a b
viewl' q = case q of
	Q0 -> TEmptyL
	Q1 a -> a :< Q0
	QN (B2 (a :* b)) m r -> a :< QN (B1 b) m r
	QN (B1 a) m r -> a :< shiftLeft m r
	where
	shiftLeft :: Queue (Pair c) a w -> Buffer c w b -> Queue c a b
	shiftLeft q' r = case viewl' q' of
		TEmptyL -> buf2queue r
		l :< m -> QN (B2 l) m r
	buf2queue (B1 a) = Q1 a
	buf2queue (B2 (a :* b)) = QN (B1 a) Q0 (B1 b)
