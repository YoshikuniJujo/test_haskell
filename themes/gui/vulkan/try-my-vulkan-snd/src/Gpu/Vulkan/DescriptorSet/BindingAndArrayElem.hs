{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.BindingAndArrayElem where

import Control.Arrow
import Data.TypeLevel.List qualified as TList

import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout

class BindingAndArrayElem
	(bts :: [Layout.BindingType]) (objs :: [VObj.Object]) where
	bindingAndArrayElem :: Integral n => n -> (n, n)

instance BindingAndArrayElem bts (oo ': os) =>
	BindingAndArrayElem ('Layout.Buffer '[] ': bts) (oo ': os) where
	bindingAndArrayElem c = (a + 1, b - c) where
		(a, b) = bindingAndArrayElem @bts @(oo ': os) 0

instance TList.IsPrefixOf os os' => BindingAndArrayElem
		('Layout.Buffer (VObj.StaticObject algn 'Nothing ot t ': os') ': bts)
		(VObj.StaticObject algn ('Just _nm) ot t ': os) where
	bindingAndArrayElem _ = (0, 0)

instance TList.IsPrefixOf os os' => BindingAndArrayElem
		('Layout.Buffer (VObj.StaticObject algn ('Just _nm) ot t ': os') ': bts)
		(VObj.StaticObject algn 'Nothing ot t ': os) where
	bindingAndArrayElem _ = (0, 0)

instance TList.IsPrefixOf os os' => BindingAndArrayElem
		('Layout.Buffer (VObj.DynamicObject n algn 'Nothing ot t ': os') ': bts)
		(VObj.DynamicObject n algn ('Just _nm) ot t ': os) where
	bindingAndArrayElem _ = (0, 0)

instance TList.IsPrefixOf os os' => BindingAndArrayElem
		('Layout.Buffer (VObj.DynamicObject n algn ('Just _nm) ot t ': os') ': bts)
		(VObj.DynamicObject n algn 'Nothing ot t ': os) where
	bindingAndArrayElem _ = (0, 0)

instance TList.IsPrefixOf os os' => BindingAndArrayElem
		('Layout.Buffer (o ': os') ': bts) (o ': os) where
	bindingAndArrayElem _ = (0, 0)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem ('Layout.Buffer os' ': bts) (oo ': os) =>
	BindingAndArrayElem
		('Layout.Buffer (o ': os') ': bts) (oo ': os) where
	bindingAndArrayElem c = (+ 1) `second`
		(bindingAndArrayElem
			@('Layout.Buffer os' ': bts) @(oo ': os) $ c + 1)
