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

-- ** BindingAndArrayElemBuffer

class BindingAndArrayElemBuffer
	(lbts :: [Layout.BindingType]) (objs :: [VObj.Object]) (i :: Nat) where
	bindingAndArrayElemBuffer :: Integral n => n -> n -> (n, n)

instance IsPrefixObject objs lobjs => BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.StaticObject algn 'Nothing ot t ': lobjs) ': lbts)
		(VObj.StaticObject algn ('Just _nm) ot t ': objs) 0 where
	bindingAndArrayElemBuffer c d = (c, d)

instance IsPrefixObject objs lobjs => BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.StaticObject algn ('Just _nm) ot t ': lobjs) ': lbts)
		(VObj.StaticObject algn 'Nothing ot t ': objs) 0 where
	bindingAndArrayElemBuffer c d = (c, d)

instance IsPrefixObject objs lobjs => BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.DynamicObject n algn 'Nothing ot t ': lobjs) ': lbts)
		(VObj.DynamicObject n algn ('Just _nm) ot t ': objs) 0 where
	bindingAndArrayElemBuffer c d = (c, d)

instance IsPrefixObject objs lobjs => BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.DynamicObject n algn ('Just _nm) ot t ': lobjs) ': lbts)
		(VObj.DynamicObject n algn 'Nothing ot t ': objs) 0 where
	bindingAndArrayElemBuffer c d = (c, d)

instance IsPrefixObject objs lobjs => BindingAndArrayElemBuffer
		('Layout.Buffer (o ': lobjs) ': lbts) (o ': objs) 0 where
	bindingAndArrayElemBuffer c d = (c, d)

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Layout.Buffer lobjs ': lbts) objs (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.StaticObject algn 'Nothing ot t ': lobjs) ': lbts)
		(VObj.StaticObject algn ('Just _nm) ot t ': objs) i where
	bindingAndArrayElemBuffer c d =
		bindingAndArrayElemBuffer @('Layout.Buffer lobjs ': lbts) @objs @(i - 1) c d

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Layout.Buffer lobjs ': lbts) objs (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.StaticObject algn ('Just _nm) ot t ': lobjs) ': lbts)
		(VObj.StaticObject algn 'Nothing ot t ': objs) i where
	bindingAndArrayElemBuffer c d =
		bindingAndArrayElemBuffer @('Layout.Buffer lobjs ': lbts) @objs @(i - 1) c d

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Layout.Buffer lobjs ': lbts) objs (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.DynamicObject n algn 'Nothing ot t ': lobjs) ': lbts)
		(VObj.DynamicObject n algn ('Just _nm) ot t ': objs) i where
	bindingAndArrayElemBuffer c d =
		bindingAndArrayElemBuffer @('Layout.Buffer lobjs ': lbts) @objs @(i - 1) c d

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Layout.Buffer lobjs ': lbts) objs (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (VObj.DynamicObject n algn ('Just _nm) ot t ': lobjs) ': lbts)
		(VObj.DynamicObject n algn 'Nothing ot t ': objs) i where
	bindingAndArrayElemBuffer c d =
		bindingAndArrayElemBuffer @('Layout.Buffer lobjs ': lbts) @objs @(i - 1) c d

instance {-# OVERLAPPABLE #-} (
	BindingAndArrayElemBuffer ('Layout.Buffer lobjs ': lbts) objs (i - 1) ) =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (o ': lobjs) ': lbts) (o ': objs) i where
	bindingAndArrayElemBuffer c d =
		bindingAndArrayElemBuffer @('Layout.Buffer lobjs ': lbts) @objs @(i - 1) c d

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemBuffer ('Layout.Buffer lobjs ': lbts) (oo ': objs) i =>
	BindingAndArrayElemBuffer
		('Layout.Buffer (o ': lobjs) ': lbts) (oo ': objs) i where
	bindingAndArrayElemBuffer c d = bindingAndArrayElemBuffer @('Layout.Buffer lobjs ': lbts) @(oo ': objs) @i c (d + 1) 

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemBuffer lbts (oo ': objs) i =>
	BindingAndArrayElemBuffer (bt ': lbts) (oo ': objs) i where
	bindingAndArrayElemBuffer c _d = bindingAndArrayElemBuffer @lbts @(oo ': objs) @i (c + 1) 0

-- ** IsPrefixObject

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
