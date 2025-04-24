{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.HigherOpenUnion (

	U, Member, HT, FromFirst(..), inj, injh, prj, decomp, extract, weaken,

	NonDet(..), Fail(..)

	) where

import Control.Monad.Freer.NonDetable qualified as NonDetable
import Control.Monad.Freer.Failable qualified as Failable
import Data.Kind
import Data.HFunctor qualified as HFunctor
import Unsafe.Coerce

type HT = (Type -> Type) -> Type -> Type

data U (hs ::[HT]) (f :: Type -> Type) a = forall h . U Word (h f a)

newtype P (h :: HT) (hs :: [HT]) = P { unP :: Word }

class Member (h :: HT) (hs :: [HT]) where elemNo :: P h hs

instance Member h (h ': hs) where elemNo = P 0

instance {-# OVERLAPPABLE #-} Member h hs => Member h (_h' ': hs) where
	elemNo = P $ 1 + unP (elemNo :: P h hs)

data FromFirst t (f :: Type -> Type) a = forall x . FromFirst (t x) (x -> a)

inj :: forall t hs f a . Member (FromFirst t) hs => t a -> U hs f a
inj = U (unP (elemNo :: P (FromFirst t) hs)) . (`FromFirst` id)

injh :: forall h hs f a . Member h hs => h f a -> U hs f a
injh = U $ unP (elemNo :: P h hs)

prj :: forall h hs f a . Member h hs => U hs f a -> Maybe (h f a)
prj (U i x)
	| i == unP (elemNo :: P h hs) = Just $ unsafeCoerce x
	| otherwise = Nothing

decomp :: U (h ': hs) f a -> Either (U hs f a) (h f a)
decomp (U 0 hx) = Right $ unsafeCoerce hx
decomp (U i hx) = Left $ U (i - 1) hx

extract :: Functor t => U '[FromFirst t] f a -> t a
extract (U _ hx) = case unsafeCoerce hx of
	FromFirst tx f -> f <$> tx

extracth :: U '[h] f a -> h f a
extracth (U _ hx) = unsafeCoerce hx

weaken :: U r f a -> U (any ': r) f a
weaken (U n a) = U (n + 1) a

instance HFunctor.H (FromFirst t) where
	map _ g (FromFirst x k) = FromFirst x (g . k)

instance HFunctor.H (U '[]) where map _ _ _ = error "never occur"

instance (HFunctor.H h, HFunctor.H (U hs)) => HFunctor.H (U (h ': hs)) where
	map :: forall f g x y .
		(f x -> g y) -> (x -> y) -> U (h ': hs) f x -> U (h ': hs) g y
	map f g u = case decomp u of
		Left u' -> weaken $ HFunctor.map f g u'
		Right h -> injh $ HFunctor.map f g h

instance Member (FromFirst NonDet) effs => NonDetable.N (U effs f) where
	mz = inj (NonDetable.mz @NonDet); mp = inj (NonDetable.mp @NonDet)

data NonDet a where MZero :: NonDet a; MPlus :: NonDet Bool

instance NonDetable.N NonDet where mz = MZero; mp = MPlus

instance Member (FromFirst Fail) effs => Failable.F (U effs f) where fail = inj . Fail

data Fail a = Fail String deriving Show
