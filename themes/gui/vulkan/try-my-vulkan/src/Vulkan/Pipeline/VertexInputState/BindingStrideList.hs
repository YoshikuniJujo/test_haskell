{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.BindingStrideList where

import GHC.Generics
import Foreign.Storable.SizeAlignment
import Data.Kind

import Vulkan.Pipeline.VertexInputState.Flatten

class BindingStrideList a where
	bindingStrideList :: [SizeAlignment]

	default bindingStrideList ::
		BindingStrideListList (Flatten (Rep a)) => [SizeAlignment]
	bindingStrideList = bindingStrideListList @(Flatten (Rep a))

instance (BindingStrideListList (Flatten (Rep a))) => BindingStrideList a

class BindingStrideListList (ts :: [Type]) where
	bindingStrideListList :: [SizeAlignment]

instance BindingStrideListList '[] where bindingStrideListList = []

instance (SizeAlignmentList t, BindingStrideListList ts) =>
	BindingStrideListList (t ': ts) where
	bindingStrideListList =
		wholeSizeAlignment @t : bindingStrideListList @ts
