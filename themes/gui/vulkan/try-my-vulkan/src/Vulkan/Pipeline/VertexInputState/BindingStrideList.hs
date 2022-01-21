{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.BindingStrideList where

import Foreign.Storable.SizeAlignment
import Data.Kind

import Vulkan.Pipeline.VertexInputState.Flatten

class BindingStrideListList (ts :: [Type]) where
	bindStrideListList :: [SizeAlignment]

instance BindingStrideListList '[] where bindStrideListList = []

instance (SizeAlignmentList t, BindingStrideListList ts) =>
	BindingStrideListList (t ': ts) where
	bindStrideListList = wholeSizeAlignment @t : bindStrideListList @ts
