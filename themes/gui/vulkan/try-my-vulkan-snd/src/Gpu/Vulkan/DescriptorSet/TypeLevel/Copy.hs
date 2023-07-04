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

import Gpu.Vulkan.TypeEnum qualified as T
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout
import Gpu.Vulkan.DescriptorSet.BindingAndArrayElem qualified as Common
import Gpu.Vulkan.DescriptorSet.TypeLevel.Common qualified as Common

class BindingAndArrayElement
	(bts :: [Layout.BindingType])
	(bt :: Layout.BindingType) (i :: Nat) where
	bindingAndArrayElement :: Integral n => (n, n)

instance Common.BindingAndArrayElem bts vobjs =>
	BindingAndArrayElement bts (Layout.Buffer vobjs) 0 where
	bindingAndArrayElement = Common.bindingAndArrayElem @bts @vobjs 0

class BindingLength (bt :: Layout.BindingType) where
	bindingLength :: Integral n => n

instance Common.BindingAndArrayElemBufferView bts nmts i =>
	BindingAndArrayElement bts (Layout.BufferView nmts) i where
	bindingAndArrayElement =
		Common.bindingAndArrayElemBufferView @bts @nmts @i 0 0

instance
	BindingAndArrayElemImage bts imgs i =>
	BindingAndArrayElement bts (Layout.Image imgs) i where
	bindingAndArrayElement = bindingAndArrayElemImage @bts @imgs @i 0 0
	

instance TypeLevel.Length.Length vobjs =>
	BindingLength (Layout.Buffer vobjs) where
	bindingLength = TypeLevel.Length.length @_ @vobjs

class IsPrefixImage
	(sfs1 :: [(Symbol, T.Format)]) (sfs2 :: [(Symbol, T.Format)])

instance IsPrefixImage '[] sfs2
instance IsPrefixImage sfs1 sfs2 => IsPrefixImage (bti ': sfs1) (bti ': sfs2)

class BindingAndArrayElemImage
	(bts :: [Layout.BindingType])
	(sfs :: [(Symbol, T.Format)]) (i :: Nat) where
	bindingAndArrayElemImage :: Integral n => n -> n -> (n, n)

instance {-# OVERLAPPABLE #-}
	IsPrefixImage sfs1 sfs2 => BindingAndArrayElemImage
		('Layout.Image (sf ': sfs2) ': bts) (sf ': sfs1) 0 where
	bindingAndArrayElemImage b ae = (b, ae)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage
		('Layout.Image sfs2 ': bts) (sf ': sfs1) (i - 1) =>
	BindingAndArrayElemImage
		('Layout.Image (sf ': sfs2) ': bts) (sf ': sfs1) i where
	bindingAndArrayElemImage b ae = bindingAndArrayElemImage
		@('Layout.Image sfs2 ': bts) @(sf ': sfs1) @(i - 1) b (ae + 1)

instance BindingAndArrayElemImage bts sfs i =>
	BindingAndArrayElemImage ('Layout.Image '[] ': bts) sfs i where
	bindingAndArrayElemImage b _ =
		bindingAndArrayElemImage @bts @sfs @i (b + 1) 0

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage ('Layout.Image nmfmts ': bts) sfs i =>
	BindingAndArrayElemImage ('Layout.Image (nmfmt ': nmfmts) ': bts) sfs i where
	bindingAndArrayElemImage b ae = bindingAndArrayElemImage
		@('Layout.Image nmfmts ': bts) @sfs @i b (ae + 1)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage bts sfs i =>
	BindingAndArrayElemImage (bt ': bts) sfs i where
	bindingAndArrayElemImage b _ =
		bindingAndArrayElemImage @bts @sfs @i (b + 1) 0
