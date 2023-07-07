{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.TypeLevel.Copy where

import Data.TypeLevel.List qualified as TypeLevel.Length

import GHC.TypeLits

import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout
import Gpu.Vulkan.DescriptorSet.BindingAndArrayElem qualified as Common
import Gpu.Vulkan.DescriptorSet.BindingAndArrayElem.Buffer qualified as Common

class BindingAndArrayElement
	(bts :: [Layout.BindingType])
	(bt :: Layout.BindingType) (i :: Nat) where
	bindingAndArrayElement :: Integral n => (n, n)

instance Common.BindingAndArrayElem bts vobjs i =>
	BindingAndArrayElement bts (Layout.Buffer vobjs) i where
	bindingAndArrayElement = Common.bindingAndArrayElem @bts @vobjs @i 0 0

instance Common.BindingAndArrayElemBufferView bts nmts i =>
	BindingAndArrayElement bts (Layout.BufferView nmts) i where
	bindingAndArrayElement =
		Common.bindingAndArrayElemBufferView @bts @nmts @i 0 0

instance
	Common.BindingAndArrayElemImage bts imgs i =>
	BindingAndArrayElement bts (Layout.Image imgs) i where
	bindingAndArrayElement = Common.bindingAndArrayElemImage @bts @imgs @i 0 0

class BindingLength (bt :: Layout.BindingType) where
	bindingLength :: Integral n => n

instance TypeLevel.Length.Length vobjs =>
	BindingLength (Layout.Buffer vobjs) where
	bindingLength = TypeLevel.Length.length @_ @vobjs
