{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList where

import Foreign.Storable.SizeAlignment
import Data.Kind

class BindingStrideListList (ts :: [Type]) k v where
	bindingStrideListList :: [(SizeAlignment, v)]

instance BindingStrideListList '[] k v where bindingStrideListList = []

instance (SizeAlignmentList t, BindingStrideListList ts k v, TypeVal a v) =>
	BindingStrideListList (AddType t (a :: k) ': ts) k v where
	bindingStrideListList =
		(wholeSizeAlignment @t, typeVal @k @a @v) : bindingStrideListList @ts @k @v

newtype AddType v t = AT v deriving Show

type family SubType t where SubType (AddType v t) = v

type family MapSubType t where
	MapSubType '[] = '[]
	MapSubType (AddType v t ': ats) = v ': MapSubType ats

type family MapUnList t where
	MapUnList '[] = '[]
	MapUnList (a ': ts) = a ': MapUnList ts

class TypeVal (t :: k) v where typeVal :: v

type Simplify a = MapUnList (MapSubType a)
