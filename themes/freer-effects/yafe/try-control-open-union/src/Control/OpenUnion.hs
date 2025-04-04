{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE ExistentialQuantification, TypeFamilies, GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.OpenUnion (
	U, Member, T, inj, prj, decomp, extract, weaken, NonDet(..),

	U', inj', prj', decomp', extract', weaken',

	Foo, Bar
	) where

import Control.NonDetable qualified as NonDetable
import Data.Kind
import Unsafe.Coerce (unsafeCoerce)

type T = Type -> Type

data U (ts :: [T]) a = forall t . U Word (t a)

data U' (ts :: [k]) (as :: [k']) = forall t . U' Word (t as)

newtype P (t :: k) (ts :: [k]) = P { unP :: Word }

class Member (t :: k) (ts :: [k]) where elemNo :: P t ts

instance Member t (t ': ts) where elemNo = P 0

instance {-# OVERLAPPABLE #-} Member t ts => Member t (_t' ': ts) where
	elemNo = P $ 1 + unP (elemNo :: P t ts)

inj :: forall t ts a . Member t ts => t a -> U ts a
inj = U $ unP (elemNo :: P t ts)

inj' :: forall t ts as . Member t ts => t as -> U' ts as
inj' = U' $ unP (elemNo :: P t ts)

prj :: forall t ts a . Member t ts => U ts a -> Maybe (t a)
prj (U i x)
	| i == unP (elemNo :: P t ts) = Just $ unsafeCoerce x
	| otherwise = Nothing

prj' :: forall t ts a . Member t ts => U' ts a -> Maybe (t a)
prj' (U' i x)
	| i == unP (elemNo :: P t ts) = Just $ unsafeCoerce x
	| otherwise = Nothing

decomp :: U (t ': ts) a -> Either (U ts a) (t a)
decomp (U 0 tx) = Right $ unsafeCoerce tx
decomp (U i tx) = Left $ U (i - 1) tx

decomp' :: U' (t ': ts) a -> Either (U' ts a) (t a)
decomp' (U' 0 tx) = Right $ unsafeCoerce tx
decomp' (U' i tx) = Left $ U' (i - 1) tx

extract :: U '[t] a -> t a
extract (U _ tx) = unsafeCoerce tx

extract' :: U' '[t] a -> t a
extract' (U' _ tx) = unsafeCoerce tx

weaken :: U r a -> U (any ': r) a
weaken (U n a) = U (n + 1) a

weaken' :: U' r a -> U' (any ': r) a
weaken' (U' n a) = U' (n + 1) a

-- instance (NonDetable nd, Member nd effs) => NonDetable (U effs) where
instance Member NonDet effs => NonDetable.N (U effs) where
	mz = inj (NonDetable.mz @NonDet); mp = inj (NonDetable.mp @NonDet)

data NonDet a where MZero :: NonDet a; MPlus :: NonDet Bool

instance NonDetable.N NonDet where mz = MZero; mp = MPlus

type Foo (ts :: [k]) = U' ts ('[] :: [Type])

type Bar ts a = U' ts '[a]
