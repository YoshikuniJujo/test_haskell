{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.BindingAndArrayElem where

import GHC.TypeLits
import Control.Arrow
import Data.Kind
import Data.TypeLevel.List qualified as TList
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex

import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.TypeEnum qualified as T
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout

-- * BUFFER

class BindingAndArrayElem
	(bts :: [Layout.BindingType]) (objs :: [VObj.Object]) where
	bindingAndArrayElem :: Integral n => n -> n -> (n, n)

instance BindingAndArrayElem bts (oo ': os) =>
	BindingAndArrayElem ('Layout.Buffer '[] ': bts) (oo ': os) where
	bindingAndArrayElem c d = bindingAndArrayElem @bts @(oo ': os) (c + 1) 0

instance IsPrefixObject os os' => BindingAndArrayElem
		('Layout.Buffer (VObj.StaticObject algn 'Nothing ot t ': os') ': bts)
		(VObj.StaticObject algn ('Just _nm) ot t ': os) where
	bindingAndArrayElem c d = (c, d)

instance IsPrefixObject os os' => BindingAndArrayElem
		('Layout.Buffer (VObj.StaticObject algn ('Just _nm) ot t ': os') ': bts)
		(VObj.StaticObject algn 'Nothing ot t ': os) where
	bindingAndArrayElem c d = (c, d)

instance IsPrefixObject os os' => BindingAndArrayElem
		('Layout.Buffer (VObj.DynamicObject n algn 'Nothing ot t ': os') ': bts)
		(VObj.DynamicObject n algn ('Just _nm) ot t ': os) where
	bindingAndArrayElem c d = (c, d)

instance IsPrefixObject os os' => BindingAndArrayElem
		('Layout.Buffer (VObj.DynamicObject n algn ('Just _nm) ot t ': os') ': bts)
		(VObj.DynamicObject n algn 'Nothing ot t ': os) where
	bindingAndArrayElem c d = (c, d)

instance IsPrefixObject os os' => BindingAndArrayElem
		('Layout.Buffer (o ': os') ': bts) (o ': os) where
	bindingAndArrayElem c d = (c, d)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem ('Layout.Buffer os' ': bts) (oo ': os) =>
	BindingAndArrayElem
		('Layout.Buffer (o ': os') ': bts) (oo ': os) where
	bindingAndArrayElem c d = bindingAndArrayElem @('Layout.Buffer os' ': bts) @(oo ': os) c (d + 1) 

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem bts (oo ': os) =>
	BindingAndArrayElem (bt ': bts) (oo ': os) where
	bindingAndArrayElem c d = bindingAndArrayElem @bts @(oo ': os) (c + 1) 0

class IsPrefixObject (objs :: [VObj.Object]) (objs' :: [VObj.Object])

instance IsPrefixObject '[] objs'

instance IsPrefixObject objs objs' => IsPrefixObject
		(VObj.StaticObject algn 'Nothing ot t ': objs)
		(VObj.StaticObject algn ('Just _nm) ot t ': objs')

instance IsPrefixObject objs objs' => IsPrefixObject
		(VObj.StaticObject algn ('Just _nm) ot t ': objs)
		(VObj.StaticObject algn 'Nothing ot t ': objs')

instance IsPrefixObject objs objs' => IsPrefixObject
		(VObj.DynamicObject n algn 'Nothing ot t ': objs)
		(VObj.DynamicObject n algn ('Just _nm) ot t ': objs')

instance IsPrefixObject objs objs' => IsPrefixObject
		(VObj.DynamicObject n algn ('Just _nm) ot t ': objs)
		(VObj.DynamicObject n algn 'Nothing ot t ': objs')

instance IsPrefixObject objs objs' =>
	IsPrefixObject (obj ': objs) (obj ': objs')

-- * IMAGE

class BindingAndArrayElemImage
	(bts :: [Layout.BindingType])
	(imgs :: [(Type, Symbol, T.Format, Type)]) where
	bindingAndArrayElemImage :: Integral n => n -> n -> (n, n)

instance TList.IsPrefixOf (TMapIndex.M1'2_4 sais) nmfmts =>
	BindingAndArrayElemImage
		('Layout.Image ('(nm, fmt) ': nmfmts) : bts)
		('(a, nm, fmt, b) ': sais) where
	bindingAndArrayElemImage b a = (b, a)

instance BindingAndArrayElemImage bts sais =>
	BindingAndArrayElemImage ('Layout.Image '[] ': bts)
	sais where
	bindingAndArrayElemImage b _ =
		bindingAndArrayElemImage @bts @sais (b + 1) 0

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage
		('Layout.Image nmfmts : bts) sais =>
	BindingAndArrayElemImage
		('Layout.Image (nmfmt ': nmfmts) ': bts) sais where
	bindingAndArrayElemImage b a = bindingAndArrayElemImage
		@('Layout.Image nmfmts : bts) @sais b (a + 1)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage bts sais =>
	BindingAndArrayElemImage (bt ': bts) sais where
	bindingAndArrayElemImage b _ = bindingAndArrayElemImage @bts @sais (b + 1) 0

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
