{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.BindingAndArrayElem (

	-- * IMAGE

	BindingAndArrayElemImage(..),

	-- * BUFFER VIEW

	BindingAndArrayElemBufferView(..) ) where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.List qualified as TList

import Gpu.Vulkan.TypeEnum qualified as T
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Lyt

-- * IMAGE

class BindingAndArrayElemImage
	(lbts :: [Lyt.BindingType])
	(iargs :: [(Symbol, T.Format)]) (i :: Nat) where
	bindingAndArrayElemImage :: Integral n => n -> n -> (n, n)

instance TList.IsPrefixOf iargs liargs =>
	BindingAndArrayElemImage
		('Lyt.Image (iarg ': liargs) : lbts) (iarg ': iargs) 0 where
	bindingAndArrayElemImage b ae = (b, ae)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage
		('Lyt.Image liargs ': lbts) (iarg ': iargs) (i - 1) =>
	BindingAndArrayElemImage
		('Lyt.Image (iarg ': liargs) ': lbts) (iarg ': iargs) i where
	bindingAndArrayElemImage b ae = bindingAndArrayElemImage
		@('Lyt.Image liargs ': lbts) @(iarg ': iargs) @(i - 1) b (ae + 1)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage ('Lyt.Image liargs : lbts) iargs i =>
	BindingAndArrayElemImage
		('Lyt.Image (liarg ': liargs) ': lbts) iargs i where
	bindingAndArrayElemImage b a = bindingAndArrayElemImage
		@('Lyt.Image liargs : lbts) @iargs @i b (a + 1)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage lbts iargs i =>
	BindingAndArrayElemImage (bt ': lbts) iargs i where
	bindingAndArrayElemImage b _ =
		bindingAndArrayElemImage @lbts @iargs @i (b + 1) 0

-- * BUFFER VIEW

class BindingAndArrayElemBufferView
	(bt :: [Lyt.BindingType]) (bvargs :: [(Symbol, Type)]) (i :: Nat) where
	bindingAndArrayElemBufferView :: Integral n => n -> n -> (n, n)

instance TList.IsPrefixOf bvargs lbvargs => BindingAndArrayElemBufferView
		('Lyt.BufferView (bvarg ': lbvargs) ': lbts)
		(bvarg ': bvargs) 0 where
	bindingAndArrayElemBufferView b a = (b, a)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemBufferView
		('Lyt.BufferView lbvargs ': lbts) (bvarg ': bvargs) (i - 1) =>
	BindingAndArrayElemBufferView
		('Lyt.BufferView (bvarg ': lbvargs) ': lbts)
		(bvarg ': bvargs) i where
	bindingAndArrayElemBufferView b a = bindingAndArrayElemBufferView
		@('Lyt.BufferView lbvargs ': lbts) @(bvarg ': bvargs) @(i - 1)
		b a

instance {-# OVERLAPPABLE #-} BindingAndArrayElemBufferView
	('Lyt.BufferView lbvargs ': lbts) bvargs i =>
	BindingAndArrayElemBufferView
		('Lyt.BufferView (bvarg ': lbvargs) ': lbts) bvargs i where
	bindingAndArrayElemBufferView b a = bindingAndArrayElemBufferView
		@('Lyt.BufferView lbvargs ': lbts) @bvargs @i b (a + 1)

instance {-# OVERLAPPABLE #-} BindingAndArrayElemBufferView lbts bvargs i =>
	BindingAndArrayElemBufferView
		(bt ': lbts) bvargs i where
	bindingAndArrayElemBufferView b _a =
		bindingAndArrayElemBufferView @lbts @bvargs @i (b + 1) 0