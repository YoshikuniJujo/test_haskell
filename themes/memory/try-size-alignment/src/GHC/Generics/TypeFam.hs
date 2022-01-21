{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.Generics.TypeFam where

import GHC.Generics
import Data.Kind

type family GetType (x :: Type -> Type) :: Type where
	GetType (K1 i a) = a
	GetType (M1 m i a) = GetType a

type family Flatten (x :: Type -> Type) :: [Type] where
	Flatten (K1 i a) = '[a]
	Flatten (M1 m i a) = Flatten a
	Flatten (M1 m i a :*: t2) = GetType a ': Flatten t2
	Flatten ((t1 :*: t2) :*: t3) = Flatten (t1 :*: t2 :*: t3)
