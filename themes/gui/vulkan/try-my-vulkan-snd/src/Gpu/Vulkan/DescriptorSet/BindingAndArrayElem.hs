{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.BindingAndArrayElem where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.List qualified as TList

import Gpu.Vulkan.TypeEnum qualified as T
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout

-- * IMAGE

class BindingAndArrayElemImage
	(bts :: [Layout.BindingType])
	(imgs :: [(Symbol, T.Format)]) (i :: Nat) where
	bindingAndArrayElemImage :: Integral n => n -> n -> (n, n)

instance TList.IsPrefixOf sais nmfmts =>
	BindingAndArrayElemImage
		('Layout.Image ('(nm, fmt) ': nmfmts) : bts)
		('(nm, fmt) ': sais) 0 where
	bindingAndArrayElemImage b a = (b, a)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage
		('Layout.Image sfs2 ': bts) ('(nm, fmt) ': sfs1) (i - 1) =>
	BindingAndArrayElemImage
		('Layout.Image ('(nm, fmt) ': sfs2) ': bts) ('(nm, fmt) ': sfs1) i where
	bindingAndArrayElemImage b ae = bindingAndArrayElemImage
		@('Layout.Image sfs2 ': bts) @('(nm, fmt) ': sfs1) @(i - 1) b (ae + 1)

instance BindingAndArrayElemImage bts sais i =>
	BindingAndArrayElemImage ('Layout.Image '[] ': bts)
	sais i where
	bindingAndArrayElemImage b _ =
		bindingAndArrayElemImage @bts @sais @i (b + 1) 0

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage
		('Layout.Image nmfmts : bts) sais i =>
	BindingAndArrayElemImage
		('Layout.Image (nmfmt ': nmfmts) ': bts) sais i where
	bindingAndArrayElemImage b a = bindingAndArrayElemImage
		@('Layout.Image nmfmts : bts) @sais @i b (a + 1)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage bts sais i =>
	BindingAndArrayElemImage (bt ': bts) sais i where
	bindingAndArrayElemImage b _ = bindingAndArrayElemImage @bts @sais @i (b + 1) 0

-- * BUFFER VIEW

class BindingAndArrayElemBufferView
	(bt :: [Layout.BindingType])
	(bvs :: [(Symbol, Type)]) (i :: Nat) where
	bindingAndArrayElemBufferView :: Integral n => n -> n -> (n, n)

instance TList.IsPrefixOf bvs bvs' =>
	BindingAndArrayElemBufferView
		('Layout.BufferView ('(nm, t) ': bvs') ': bts)
		('(nm, t) ': bvs) 0 where
	bindingAndArrayElemBufferView b a = (b, a)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemBufferView
		('Layout.BufferView bvs' ': bts)
		('(nm, t) ': bvs) (i - 1) =>
	BindingAndArrayElemBufferView
		('Layout.BufferView ('(nm, t) ': bvs') ': bts)
		('(nm, t) ': bvs) i where
	bindingAndArrayElemBufferView b a = bindingAndArrayElemBufferView
		@('Layout.BufferView bvs' ': bts)
		@('(nm, t) ': bvs) @(i - 1) b a

instance {-# OVERLAPPABLE #-} BindingAndArrayElemBufferView
	('Layout.BufferView bvs' ': bts) bvs i =>
	BindingAndArrayElemBufferView
		('Layout.BufferView (nmt ': bvs') ': bts) bvs i where
	bindingAndArrayElemBufferView b a = bindingAndArrayElemBufferView
		@('Layout.BufferView bvs' ': bts) @bvs @i b (a + 1)

instance {-# OVERLAPPABLE #-} BindingAndArrayElemBufferView bts bvs i =>
	BindingAndArrayElemBufferView
--	('Layout.BufferView '[] ': bts) bvs where
		(bt ': bts) bvs i where
	bindingAndArrayElemBufferView b _a =
		bindingAndArrayElemBufferView @bts @bvs @i (b + 1) 0
