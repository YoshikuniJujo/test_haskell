{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.BindingAndArrayElem.Buffer where

import GHC.TypeLits

import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout

-- * BUFFER

class BindingAndArrayElemBuffer
	(bts :: [Layout.BindingType]) (objs :: [VObj.Object]) (i :: Nat) where
	bindingAndArrayElem :: Integral n => n -> n -> (n, n)

instance IsPrefixObject os os' => BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.StaticObject algn 'Nothing ot t ': os') ': bts)
		(VObj.StaticObject algn ('Just _nm) ot t ': os) 0 where
	bindingAndArrayElem c d = (c, d)

instance IsPrefixObject os os' => BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.StaticObject algn ('Just _nm) ot t ': os') ': bts)
		(VObj.StaticObject algn 'Nothing ot t ': os) 0 where
	bindingAndArrayElem c d = (c, d)

instance IsPrefixObject os os' => BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.DynamicObject n algn 'Nothing ot t ': os') ': bts)
		(VObj.DynamicObject n algn ('Just _nm) ot t ': os) 0 where
	bindingAndArrayElem c d = (c, d)

instance IsPrefixObject os os' => BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.DynamicObject n algn ('Just _nm) ot t ': os') ': bts)
		(VObj.DynamicObject n algn 'Nothing ot t ': os) 0 where
	bindingAndArrayElem c d = (c, d)

instance IsPrefixObject os os' => BindingAndArrayElemBuffer
		('Layout.Buffer (o ': os') ': bts) (o ': os) 0 where
	bindingAndArrayElem c d = (c, d)

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Layout.Buffer os' ': bts) os (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.StaticObject algn 'Nothing ot t ': os') ': bts)
		(VObj.StaticObject algn ('Just _nm) ot t ': os) i where
	bindingAndArrayElem c d =
		bindingAndArrayElem @('Layout.Buffer os' ': bts) @os @(i - 1) c d

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Layout.Buffer os' ': bts) os (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.StaticObject algn ('Just _nm) ot t ': os') ': bts)
		(VObj.StaticObject algn 'Nothing ot t ': os) i where
	bindingAndArrayElem c d =
		bindingAndArrayElem @('Layout.Buffer os' ': bts) @os @(i - 1) c d

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Layout.Buffer os' ': bts) os (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.DynamicObject n algn 'Nothing ot t ': os') ': bts)
		(VObj.DynamicObject n algn ('Just _nm) ot t ': os) i where
	bindingAndArrayElem c d =
		bindingAndArrayElem @('Layout.Buffer os' ': bts) @os @(i - 1) c d

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Layout.Buffer os' ': bts) os (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.DynamicObject n algn ('Just _nm) ot t ': os') ': bts)
		(VObj.DynamicObject n algn 'Nothing ot t ': os) i where
	bindingAndArrayElem c d =
		bindingAndArrayElem @('Layout.Buffer os' ': bts) @os @(i - 1) c d

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Layout.Buffer os' ': bts) os (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (o ': os') ': bts) (o ': os) i where
	bindingAndArrayElem c d =
		bindingAndArrayElem @('Layout.Buffer os' ': bts) @os @(i - 1) c d

instance BindingAndArrayElemBuffer bts (oo ': os) i =>
	BindingAndArrayElemBuffer ('Layout.Buffer '[] ': bts) (oo ': os) i where
	bindingAndArrayElem c _d = bindingAndArrayElem @bts @(oo ': os) @i (c + 1) 0

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemBuffer ('Layout.Buffer os' ': bts) (oo ': os) i =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (o ': os') ': bts) (oo ': os) i where
	bindingAndArrayElem c d = bindingAndArrayElem @('Layout.Buffer os' ': bts) @(oo ': os) @i c (d + 1) 

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemBuffer bts (oo ': os) i =>
	BindingAndArrayElemBuffer (bt ': bts) (oo ': os) i where
	bindingAndArrayElem c _d = bindingAndArrayElem @bts @(oo ': os) @i (c + 1) 0

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
