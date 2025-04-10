{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.OpenUnion (
	U, Member, Base, T, inj, injBase, prj, decomp, extract, weaken, NonDet(..), Fail(..)
	) where

import Control.Monad.Freer.NonDetable qualified as NonDetable
import Control.Monad.Freer.Failable qualified as Failable
import Data.Kind
import Unsafe.Coerce (unsafeCoerce)

type T = Type -> Type

data U (ts :: [T]) a = forall t . U Word (t a)

newtype P (t :: T) (ts :: [T]) = P { unP :: Word }

class Member (t :: T) (ts :: [T]) where elemNo :: P t ts

instance Member t (t ': ts) where elemNo = P 0

instance {-# OVERLAPPABLE #-} Member t ts => Member t (_t' ': ts) where
	elemNo = P $ 1 + unP (elemNo :: P t ts)

class Base (t :: T) (ts :: [T]) where elemNoBase :: P t ts

instance Base t '[t] where elemNoBase = P 0

instance Base t (v ': ts) => Base t (_u ': v ': ts) where
	elemNoBase = P $ 1 + unP (elemNoBase :: P t (v ': ts))

inj :: forall t ts a . Member t ts => t a -> U ts a
inj = U $ unP (elemNo :: P t ts)

injBase :: forall t ts a . Base t ts => t a -> U ts a
injBase = U $ unP (elemNoBase :: P t ts)

prj :: forall t ts a . Member t ts => U ts a -> Maybe (t a)
prj (U i x)
	| i == unP (elemNo :: P t ts) = Just $ unsafeCoerce x
	| otherwise = Nothing

decomp :: U (t ': ts) a -> Either (U ts a) (t a)
decomp (U 0 tx) = Right $ unsafeCoerce tx
decomp (U i tx) = Left $ U (i - 1) tx

extract :: U '[t] a -> t a
extract (U _ tx) = unsafeCoerce tx

weaken :: U r a -> U (any ': r) a
weaken (U n a) = U (n + 1) a

-- instance (NonDetable nd, Member nd effs) => NonDetable (U effs) where
instance Member NonDet effs => NonDetable.N (U effs) where
	mz = inj (NonDetable.mz @NonDet); mp = inj (NonDetable.mp @NonDet)

data NonDet a where MZero :: NonDet a; MPlus :: NonDet Bool

instance NonDetable.N NonDet where mz = MZero; mp = MPlus

data Fail a = Fail String deriving Show

instance Member Fail effs => Failable.F (U effs) where fail = inj . Fail
