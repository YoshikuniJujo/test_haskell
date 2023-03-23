{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList where

import Foreign.Storable.SizeAlignment
import Data.Kind

class BindingStrideList k (ts :: [(Type, k)]) v where
	bindingStrideList :: [(SizeAlignment, v)]

instance BindingStrideList k '[] v where bindingStrideList = []

instance (SizeAlignmentList t, BindingStrideList k ts v, TypeVal a v) =>
	BindingStrideList k (AddType t (a :: k) ': ts) v where
	bindingStrideList = (wholeSizeAlignment @t, typeVal @k @a @v) :
		bindingStrideList @k @ts @v

-- newtype v `AddType` t = AT v deriving Show

type v `AddType` t = '(v, t)

type family SubType t where SubType (v `AddType` t) = v

type family MapSubType t where
	MapSubType '[] = '[]
	MapSubType (v `AddType` t ': ats) = v ': MapSubType ats

class TypeVal (t :: k) v where typeVal :: v
