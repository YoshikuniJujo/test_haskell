{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.TypeLevel.Copy where

import Data.TypeLevel.Length qualified as TypeLevel.Length

import GHC.TypeLits

import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout
import Gpu.Vulkan.DescriptorSet.TypeLevel.Common qualified as Common

class BindingAndArrayElement
	(bts :: [Layout.BindingType])
	(bt :: Layout.BindingType) (i :: Nat) where
	bindingAndArrayElement :: Integral n => (n, n)

instance Common.BindingAndArrayElem bts vobjs i =>
	BindingAndArrayElement bts (Layout.Buffer vobjs) i where
	bindingAndArrayElement = Common.bindingAndArrayElem @bts @vobjs @i 0

class BindingLength (bt :: Layout.BindingType) where
	bindingLength :: Integral n => n

instance TypeLevel.Length.Length vobjs =>
	BindingLength (Layout.Buffer vobjs) where
	bindingLength = TypeLevel.Length.length @_ @vobjs
