{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.Flatten where

import GHC.Generics
import Data.Kind

type family GetType (x :: Type -> Type) :: Type where
	GetType (K1 i a) = a
	GetType (M1 m i a) = GetType a

type family Flatten (x :: Type -> Type) :: [Type] where
	Flatten U1 = '[]
	Flatten (K1 i a) = '[a]
	Flatten (M1 m i a) = Flatten a
	Flatten (M1 m i a :*: ts) = GetType a ': Flatten ts
	Flatten ((t1 :*: t2) :*: t3) = Flatten (t1 :*: t2 :*: t3)

type family MapType (f :: Type -> Type) (xs :: [Type]) :: [Type] where
	MapType _ '[] = '[]
	MapType f (x ': xs) = f x ': MapType f xs
