{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.OpenUnion (

	U, Member, HT, FromFirst(..),
	inj, injh, prj, decomp, extract, extracth, weaken,
	HFunctor(..)

	) where

import Data.Kind
import Yaftee.TypeElem
import Unsafe.Coerce

type HT = (Type -> Type -> Type -> Type) -> Type -> Type -> Type -> Type

data U (hs :: [HT]) (f :: Type -> Type -> Type -> Type) i o a =
	forall h . U Word (h f i o a)

data FromFirst t (f :: Type -> Type -> Type -> Type) i o a
	= forall x . FromFirst (t x) (x -> a)

class HFunctor h where
	hmap :: (f i o x -> g i o y) -> (x -> y) -> h f i o x -> h g i o y

instance HFunctor (FromFirst t) where
	hmap _ g (FromFirst x h) = FromFirst x (g . h)

instance HFunctor (U '[]) where hmap _ _ _ = error "bad"

instance (HFunctor h, HFunctor (U hs)) => HFunctor (U (h ': hs)) where
--	hmap :: forall f g i o x y .
--		(f i o x -> g i o y) -> (x -> y) -> h f i o x -> h g i o y
	hmap f g u = case decomp u of
		Left u' -> weaken $ hmap f g u'
		Right h -> injh $ hmap f g h

inj :: forall t hs (f :: Type -> Type -> Type -> Type) i o a .
	Member (FromFirst t) hs => t a -> U hs f i o a
inj tx = U (unP (elemNo :: P (FromFirst t) hs)) $ FromFirst tx id

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
