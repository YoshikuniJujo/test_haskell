{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenUnion (
	U, Member, T, FromFirst(..), inj, injh, prj, decomp, extract, weaken,
	HFunctor(..),

	NonDet(..)
	) where

import Control.Monad.Freer.NonDetable qualified as NonDetable
import Data.Kind
import Unsafe.Coerce

type T = (Type -> Type) -> Type -> Type

data U (hs :: [T]) (f :: Type -> Type) a = forall h . U Word EffectOrder (h f a)

data EffectOrder = FirstOrder | HigherOrder deriving (Show, Eq, Ord)

newtype P (h :: T) (hs :: [T]) = P { unP :: Word }

class Member (h :: T) (hs :: [T]) where elemNo :: P h hs

instance Member h (h ': hs) where elemNo = P 0

instance {-# OVERLAPPABLE #-} Member h hs => Member h (_h' : hs) where
	elemNo = P $ 1 + unP (elemNo :: P h hs)

newtype FromFirst t (f :: Type -> Type) a = FromFirst { toFirst :: t a }

inj :: forall t hs (f :: Type -> Type) a . Member (FromFirst t) hs => t a -> U hs f a
inj = U (unP (elemNo :: P (FromFirst t) hs)) FirstOrder . FromFirst @t @f

injh :: forall h hs f a . Member h hs => h f a -> U hs f a
injh = U (unP (elemNo :: P h hs)) HigherOrder

prj :: forall h hs f a . Member h hs => U hs f a -> Maybe (h f a)
prj (U i _o x)
	| i == unP (elemNo :: P h hs) = Just $ unsafeCoerce x
	| otherwise = Nothing

decomp :: U (h ': hs) f a -> Either (U hs f a) (h f a)
decomp (U 0 _o hfx) = Right $ unsafeCoerce hfx
decomp (U i o hfx) = Left $ U (i - 1) o hfx

extract :: U '[FromFirst t] f a -> t a
extract (U _ _ hfx) = toFirst $ unsafeCoerce hfx

extracth :: U '[h] f a -> h f a
extracth (U _ _ hfx) = unsafeCoerce hfx

weaken :: U r f a -> U (any ': r) f a
weaken (U n o a) = U (n + 1) o a

class HFunctor h where
	hmap :: (f x -> g y) -> (x -> y) -> h f x -> h g y

instance Functor t => HFunctor (FromFirst t) where
	hmap f g (FromFirst x) = FromFirst $ g <$> x

instance HFunctor (U '[]) where
	hmap _ _ _ = error "bad"
		
instance HFunctor h => HFunctor (U '[h]) where
	hmap :: forall f g x y . (f x -> g y) -> (x -> y) -> U '[h] f x -> U '[h] g y
	hmap f g u = injh $ hmap f g (extracth u :: h f x)

instance (HFunctor h, HFunctor (U hs)) => HFunctor (U (h ': hs)) where
	hmap :: forall f g x y .
		(f x -> g y) -> (x -> y) -> U (h ': hs) f x -> U (h ': hs) g y
	hmap f g u = case decomp u of
		Left u' -> weaken $ hmap f g u'
		Right h -> injh $ hmap f g h

instance Member (FromFirst NonDet) effs => NonDetable.N (U effs f) where
	mz = inj (NonDetable.mz @NonDet); mp = inj (NonDetable.mp @NonDet)

data NonDet a where MZero :: NonDet a; MPlus :: NonDet Bool

instance NonDetable.N NonDet where mz = MZero; mp = MPlus
