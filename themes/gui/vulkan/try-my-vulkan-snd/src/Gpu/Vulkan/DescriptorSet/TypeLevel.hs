{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.TypeLevel where

import Control.Arrow
import Data.Kind.Object

import Gpu.Vulkan.DescriptorSet

import qualified Gpu.Vulkan.Descriptor as Descriptor
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout

type family BindingTypesFromLayoutArg (tbts :: LayoutArg) ::
	[DescriptorSetLayout.BindingType] where
	BindingTypesFromLayoutArg '(t, bts) = bts

type family ObjectsFromBufferInfoArgs (bias :: [Descriptor.BufferInfoArg]) ::
	[Object] where
	ObjectsFromBufferInfoArgs '[] = '[]
	ObjectsFromBufferInfoArgs ('(sb, sm, objs, obj) ': args) =
		obj ': ObjectsFromBufferInfoArgs args

class IsPrefix (objs :: [Object]) (objs' :: [Object])

instance IsPrefix '[] objs

instance IsPrefix os os' => IsPrefix (o : os) (o : os')

class BindingAndArrayElem
	(bts :: [DescriptorSetLayout.BindingType]) (objs :: [Object]) where
	bindingAndArrayElem :: Int -> (Int, Int)

instance IsPrefix os os' =>
	BindingAndArrayElem ('DescriptorSetLayout.Buffer (o ': os') ': bts) (o ': os) where
	bindingAndArrayElem _ = (0, 0)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer os' ': bts) os =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer (o ': os') ': bts) os where
	bindingAndArrayElem c = (+ 1) `second`
		(bindingAndArrayElem
			@('DescriptorSetLayout.Buffer os' ': bts) @os $ c + 1)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem bts os =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer '[] ': bts) os where
	bindingAndArrayElem c = (a + 1, b - c) where (a, b) = bindingAndArrayElem @bts @os 0

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem bts os =>
	BindingAndArrayElem
		('DescriptorSetLayout.Other ': bts) os where
	bindingAndArrayElem c = (a + 1, b - c) where (a, b) = bindingAndArrayElem @bts @os 0

type SampleBts0 = '[
	'DescriptorSetLayout.Buffer '[ ],
	'DescriptorSetLayout.Buffer '[ 'Atom Double, 'List (), 'Atom Bool],
	'DescriptorSetLayout.Other,
	'DescriptorSetLayout.Buffer '[ 'Atom Bool, 'List (), 'Atom Int, 'List Double, 'Atom Double]]

type SampleObjs0 = '[ 'Atom Int, 'List Double]
