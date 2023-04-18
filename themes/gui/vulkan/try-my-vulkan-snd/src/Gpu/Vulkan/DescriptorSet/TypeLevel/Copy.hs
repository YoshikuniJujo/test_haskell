{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.TypeLevel.Copy where

import GHC.TypeLits

import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout
import Gpu.Vulkan.DescriptorSet.TypeLevel.Common qualified as Common

class BindingAndArrayElement
	(bts :: [Layout.BindingType])
	(bt :: Layout.BindingType) (i :: Nat) where
	bindingAndArrayElement :: Integral n => (n, n)
