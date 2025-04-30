{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.OpenUnion (

	U, Member, Base, HT, FromFirst(..),
	inj, injBase, injh, prj, decomp, extract, extracth, weaken, weaken1,

	Fail(..)

	) where

import Data.Kind
import Yaftee.TypeElem
import Unsafe.Coerce

import Control.Monad.Freer.NonDetable qualified as NonDetable
import Control.Monad.Freer.Failable qualified as Failable

type HT = (Type -> Type -> Type -> Type) -> Type -> Type -> Type -> Type

data U (hs :: [HT]) (f :: Type -> Type -> Type -> Type) i o a =
	forall h . U Word (h f i o a)

data FromFirst t (f :: Type -> Type -> Type -> Type) i o a
	= forall x . FromFirst (t x) (x -> a)

inj :: forall t hs (f :: Type -> Type -> Type -> Type) i o a .
	Member (FromFirst t) hs => t a -> U hs f i o a
inj tx = U (unP (elemNo :: P (FromFirst t) hs)) $ FromFirst tx id

injBase :: forall t hs (f :: Type -> Type -> Type -> Type) i o a .
	Base (FromFirst t) hs => t a -> U hs f i o a
injBase tx = U (unP (elemNoBase :: P (FromFirst t) hs)) $ FromFirst tx id

injh :: forall h hs f i o a . Member h hs => h f i o a -> U hs f i o a
injh = U (unP (elemNo :: P h hs))

prj :: forall h hs f i o a . Member h hs => U hs f i o a -> Maybe (h f i o a)
prj (U i hx)
	| i == unP (elemNo :: P h hs) = Just $ unsafeCoerce hx
	| otherwise = Nothing

decomp :: U (h ': hs) f i o a -> Either (U hs f i o a) (h f i o a)
decomp (U 0 hx) = Right $ unsafeCoerce hx
decomp (U i hx) = Left $ U (i - 1) hx

extract :: Functor t => U '[FromFirst t] f i o a -> t a
extract (U _ hx) = case (unsafeCoerce hx) of
	FromFirst tx k -> k <$> tx

extracth :: U '[h] f i o a -> h f i o a
extracth (U _ hx) = unsafeCoerce hx

weaken :: U r f i o a -> U (any ': r) f i o a
weaken (U n a) = U (n + 1) a

weaken1 :: U (x ': r) f i o a -> U (x ': any ': r) f i o a
weaken1 (U 0 a) = U 0 a
weaken1 (U n a) = U (n + 1) a

instance Member (FromFirst NonDet) effs => NonDetable.N (U effs f i o) where
	mz = inj (NonDetable.mz @NonDet); mp = inj (NonDetable.mp @NonDet)

data NonDet a where MZero :: NonDet a; MPlus :: NonDet Bool

instance NonDetable.N NonDet where mz = MZero; mp = MPlus

instance Member (FromFirst Fail) effs => Failable.F (U effs f i o) where fail = inj . Fail

data Fail a = Fail String deriving Show
