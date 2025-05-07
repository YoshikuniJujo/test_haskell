{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.HFunctor where

import Yaftee.OpenUnion

class HFunctor h where
	hmap :: (forall i' o' x' . f i' o' x' -> g i' o' (t x')) -> (forall x' . x' -> t x') -> h f i o x -> h g i o (t x)

--	default hmap :: HFunctorI h => (forall i' o' x' . f i' o' x' -> g i' o' (t x')) -> (forall x' . x' -> t x') -> h f i o x -> h g i o (t x)
--	hmap = hmapI

	default hmap :: HFunctor' h => (forall i' o' x' . f i' o' x' -> g i' o' (t x')) -> (forall x' . x' -> t x') -> h f i o x -> h g i o (t x)
	hmap = hmap'

instance HFunctor NonDet
instance HFunctor (FromFirst t)
instance HFunctor (U '[])

instance (HFunctor h, HFunctor (U hs)) => HFunctor (U (h ': hs)) where
	hmap f g u = case decomp u of
		Left u' -> weaken $ hmap f g u'
		Right h -> injh $ hmap f g h

class HFunctor' h where
	hmap' :: (f i o x -> g i' o' y) -> (x -> y) -> h f i o x -> h g i' o' y
--	hmap' :: (forall x' . f i o x' -> g i' o' (t x')) -> (forall x' . x' -> t x') -> h f i o x -> h g i' o' (t x)

instance HFunctor' NonDet where
	hmap' _ _ MZero = MZero
	hmap' _ g (MPlus k) = MPlus $ g . k

instance HFunctor' (FromFirst t) where
	hmap' _ g (FromFirst x h) = FromFirst x (g . h)

instance HFunctor' (U '[]) where hmap' _ _ _ = error "bad"

instance (HFunctor' h, HFunctor' (U hs)) => HFunctor' (U (h ': hs)) where
--	hmap' :: forall f g i o x y .
--		(f i o x -> g i o y) -> (x -> y) -> h f i o x -> h g i o y
	hmap' f g u = case decomp u of
		Left u' -> weaken $ hmap' f g u'
		Right h -> injh $ hmap' f g h

class HFunctorI h where
	hmapI :: (forall o' x' . f i o' x' -> g i' o' (t x')) ->
		(forall x' . x' -> t x') -> h f i o x -> h g i' o (t x)

	default hmapI :: HFunctor' h =>
		(forall o' x' . f i o' x' -> g i' o' (t x')) ->
		(forall x' . x' -> t x') -> h f i o x -> h g i' o (t x)
	hmapI = hmap'

instance HFunctorI (FromFirst t)
instance HFunctorI (U '[])

instance (HFunctorI h, HFunctorI (U hs)) => HFunctorI (U (h ': hs)) where
	hmapI f g u = case decomp u of
		Left u' -> weaken $ hmapI f g u'
		Right h -> injh $ hmapI f g h

class HFunctorO h where
	hmapO :: (forall i' x' . f i' o x' -> g i' o' (t x')) ->
		(forall x' . x' -> t x') -> h f i o x -> h g i o' (t x)

	default hmapO :: HFunctor' h =>
		(forall i' x' . f i' o x' -> g i' o' (t x')) ->
		(forall x' . x' -> t x') -> h f i o x -> h g i o' (t x)
	hmapO = hmap'

instance HFunctorO (FromFirst t)
instance HFunctorO (U '[])

instance (HFunctorO h, HFunctorO (U hs)) => HFunctorO (U (h ': hs)) where
	hmapO f g u = case decomp u of
		Left u' -> weaken $ hmapO f g u'
		Right h -> injh $ hmapO f g h

-----

{-
class HFunctorSimple h where
	hmapS ::
		(forall i' o' x' . f i' o' x' -> g i' o' x') ->
		(forall i' o' x' . g i' o' x' -> f i' o' x') ->
		h f i o x -> h g i o x

	default hmapS :: HFunctor' h =>
		(forall i' o' x' . f i' o' x' -> g i' o' x') ->
		(forall i' o' x' . g i' o' x' -> f i' o' x') ->
		h f i o x -> h g i o x
	hmapS f _ = hmap' f id

class HFunctorMoreSimple h where
	hmapMS ::
		(forall i' o' x' . f i' o' x' -> g i' o' x') ->
		h f i o x -> h g i o x

	default hmapMS :: HFunctor' h =>
		(forall i' o' x' . f i' o' x' -> g i' o' x') ->
		h f i o x -> h g i o x
	hmapMS f = hmap' f id
	-}

{-
class HFunctorStrict h where
	hmapStr ::
		(forall i' o' x' . U (eff ': effs) i' o' x' -> U effs i' o' x') ->
		h (U (eff ': effs)) i o x -> h (U effs) i o x
		-}

class HMappable h where
	hhmap :: (f i o x -> g i o x) -> h f i o x -> h g i o x

hmap2 :: (HMappable h, Functor (h g i o)) =>
	(f i o x -> g i o x) -> (x -> y) -> h f i o x -> h g i o y
hmap2 f g = fmap g . hhmap f
