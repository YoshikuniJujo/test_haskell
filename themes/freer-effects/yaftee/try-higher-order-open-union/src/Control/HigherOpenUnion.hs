{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.HigherOpenUnion (

	-- * TYPES

	U, HT, Member, Base, FromFirst(..),

	-- * INJECTION

	inj, injBase, injh,

	-- * PROJECTION

	prj, decomp, extract, extracth,

	-- * WEAKEN

	weaken,

	-- * NON DET AND FAIL

	NonDet(..), Fail(..)

	) where

import Control.Monad.Freer.NonDetable qualified as NonDetable
import Control.Monad.Freer.Failable qualified as Failable
import Data.Kind
import Data.TypeElem
import Unsafe.Coerce

-- * TYPES

data U (hs :: [HT]) (f :: Type -> Type -> Type -> Type) i o a =
	forall h . U Word (h f i o a)

type HT = (Type -> Type -> Type -> Type) -> Type -> Type -> Type -> Type

data FromFirst t (f :: Type -> Type -> Type -> Type) i o a
	= forall x . FromFirst (t x) (x -> a)

-- * INJECTION

inj :: forall t hs (f :: Type -> Type -> Type -> Type) i o a .
	Member (FromFirst t) hs => t a -> U hs f i o a
inj = U (unP (elemNo :: P (FromFirst t) hs)) . (`FromFirst` id)

injBase :: forall t hs (f :: Type -> Type -> Type -> Type) i o a .
	Base (FromFirst t) hs => t a -> U hs f i o a
injBase = U (unP (elemNoBase :: P (FromFirst t) hs)) . (`FromFirst` id)

injh :: forall h hs f i o a . Member h hs => h f i o a -> U hs f i o a
injh = U $ unP (elemNo :: P h hs)

-- * PROJECTION

prj :: forall h hs f i o a . Member h hs => U hs f i o a -> Maybe (h f i o a)
prj (U i hx)
	| i == unP (elemNo :: P h hs) = Just $ unsafeCoerce hx
	| otherwise = Nothing

decomp :: U (h ': hs) f i o a -> Either (U hs f i o a) (h f i o a)
decomp (U 0 hx) = Right $ unsafeCoerce hx
decomp (U i hx) = Left $ U (i - 1) hx

extract :: Functor t => U '[FromFirst t] f i o a -> t a
extract (U _ hx) = case (unsafeCoerce hx) of FromFirst tx k -> k <$> tx

extracth :: U '[h] f i o a -> h f i o a
extracth (U _ hx) = unsafeCoerce hx

-- * WEAKEN

weaken :: U hs f i o a -> U (any ': hs) f i o a
weaken (U n a) = U (n + 1) a

-- * NON DET AND FAIL

instance Member (FromFirst NonDet) effs => NonDetable.N (U effs f i o) where
	mz = inj (NonDetable.mz @NonDet); mp = inj (NonDetable.mp @NonDet)

data NonDet a where MZero :: NonDet a; MPlus :: NonDet Bool

deriving instance Show (NonDet a)

instance NonDetable.N NonDet where mz = MZero; mp = MPlus

instance Member Fail effs => Failable.F (U effs f i o) where
	fail = injh . Fail

data Fail (f :: Type -> Type -> Type -> Type) i o a where
	Fail :: forall f a i o . String -> Fail f i o a
	FailCatch :: forall f a i o .
		f i o a -> (String -> f i o a) -> Fail f i o a
