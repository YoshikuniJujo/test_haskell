{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ExistentialQuantification, GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OpenUnion where

import Data.Kind
import Unsafe.Coerce (unsafeCoerce)

import NonDetable

type T = Type -> Type

data Union (ts :: [T]) a = forall t . Union Word (t a)

newtype P (t :: T) (ts :: [T]) = P { unP :: Word }

class Member (t :: T) (ts :: [T]) where elemNo :: P t ts

instance Member t (t ': ts) where elemNo = P 0

instance {-# OVERLAPPABLE #-} Member t ts => Member t (_t' ': ts) where
	elemNo = P $ 1 + unP (elemNo :: P t ts)

inj :: forall t ts a . Member t ts => t a -> Union ts a
inj = Union $ unP (elemNo :: P t ts)

prj :: forall t ts a . Member t ts => Union ts a -> Maybe (t a)
prj (Union i x)
	| i == unP (elemNo :: P t ts) = Just $ unsafeCoerce x
	| otherwise = Nothing

decomp :: Union (t ': ts) a -> Either (Union ts a) (t a)
decomp (Union 0 tx) = Right $ unsafeCoerce tx
decomp (Union i tx) = Left $ Union (i - 1) tx

extract :: Union '[t] a -> t a
extract (Union _ tx) = unsafeCoerce tx

weaken :: Union r a -> Union (any ': r) a
weaken (Union n a) = Union (n + 1) a

-- instance (NonDetable nd, Member nd effs) => NonDetable (Union effs) where
instance Member NonDet effs => NonDetable (Union effs) where
	mz = inj (mz @NonDet); mp = inj (mp @NonDet)

data NonDet a where MZero :: NonDet a; MPlus :: NonDet Bool

instance NonDetable NonDet where mz = MZero; mp = MPlus
