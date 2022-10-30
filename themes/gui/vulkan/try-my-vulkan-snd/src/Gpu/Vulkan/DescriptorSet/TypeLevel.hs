{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.TypeLevel where

import GHC.TypeLits
import Control.Arrow
import Data.Kind
import Data.Kind.Object

import {-# SOURCE #-} Gpu.Vulkan.DescriptorSet

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Descriptor as Descriptor
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout

bindingAndArrayElem' ::
	forall (tbts :: LayoutArg) (bias :: [Descriptor.BufferInfoArg]) n . (
	BindingAndArrayElem
		(BindingTypesFromLayoutArg tbts)
		(ObjectsFromBufferInfoArgs bias), Integral n ) => (n, n)
bindingAndArrayElem' = bindingAndArrayElem
	@(BindingTypesFromLayoutArg tbts)
	@(ObjectsFromBufferInfoArgs bias) 0

type family BindingTypesFromLayoutArg (tbts :: LayoutArg) ::
	[DescriptorSetLayout.BindingType] where
	BindingTypesFromLayoutArg '(t, bts) = bts

type family ObjectsFromBufferInfoArgs (bias :: [Descriptor.BufferInfoArg]) ::
	[Object] where
	ObjectsFromBufferInfoArgs '[] = '[]
	ObjectsFromBufferInfoArgs ('(sb, sm, nm, objs, obj) ': args) =
		obj ': ObjectsFromBufferInfoArgs args

class IsPrefix (objs :: [Object]) (objs' :: [Object])

instance IsPrefix '[] objs

instance IsPrefix os os' => IsPrefix (o : os) (o : os')

class BindingAndArrayElem
	(bts :: [DescriptorSetLayout.BindingType]) (objs :: [Object]) where
	bindingAndArrayElem :: Integral n => n -> (n, n)

instance IsPrefix os os' =>
	BindingAndArrayElem ('DescriptorSetLayout.Buffer ('Atom algn t 'Nothing ': os') ': bts) ('Atom algn t 'Nothing ': os) where
	bindingAndArrayElem _ = (0, 0)

instance IsPrefix os os' =>
	BindingAndArrayElem ('DescriptorSetLayout.Buffer ('Atom algn t 'Nothing ': os') ': bts) ('Atom algn t ('Just nm) ': os) where
	bindingAndArrayElem _ = (0, 0)

instance IsPrefix os os' =>
	BindingAndArrayElem ('DescriptorSetLayout.Buffer ('Atom algn t ('Just nm) ': os') ': bts) ('Atom algn t 'Nothing ': os) where
	bindingAndArrayElem _ = (0, 0)

instance IsPrefix os os' =>
	BindingAndArrayElem ('DescriptorSetLayout.Buffer ('Atom algn t ('Just nm) ': os') ': bts) ('Atom algn t ('Just nm) ': os) where
	bindingAndArrayElem _ = (0, 0)

instance IsPrefix os os' =>
	BindingAndArrayElem ('DescriptorSetLayout.Buffer ('List t nm ': os') ': bts) ('List t nm ': os) where
	bindingAndArrayElem _ = (0, 0)

instance IsPrefix os os' =>
	BindingAndArrayElem ('DescriptorSetLayout.Buffer ('ObjImage t nm ': os') ': bts) ('ObjImage t nm ': os) where
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
	'DescriptorSetLayout.Buffer '[ 'Atom 256 Double 'Nothing, 'List () "", 'Atom 256 Bool 'Nothing],
	'DescriptorSetLayout.Other,
	'DescriptorSetLayout.Buffer
		'[ 'Atom 256 Bool 'Nothing, 'List () "", 'Atom 256 Int 'Nothing, 'List Double "", 'Atom 256 Double 'Nothing]]

type SampleObjs0 = '[ 'Atom 256 Int 'Nothing, 'List Double ""]

type SampleBts1 = '[
	'DescriptorSetLayout.Buffer '[ 'List Double "", 'List Double "", 'List Double ""],
	'DescriptorSetLayout.Buffer '[ 'Atom 256 Double 'Nothing ] ]

type SampleObj1 = '[ 'Atom 256 Double 'Nothing]

class IsPrefixImage
	(sais :: [(Type, T.Format, Symbol, Type)])
	(btis :: [(Symbol, T.Format)])

instance IsPrefixImage '[] bit

instance IsPrefixImage sais btis =>
	IsPrefixImage ('(a, fmt, nm, b) ': sais) ('(nm, fmt) ': btis)

class BindingAndArrayElemImage
	(bts :: [DescriptorSetLayout.BindingType])
	(imgs :: [(Type, T.Format, Symbol, Type)]) where
	bindingAndArrayElemImage :: Integral n => n -> n -> (n, n)

instance IsPrefixImage sais nmfmts =>
	BindingAndArrayElemImage
		('DescriptorSetLayout.Image ('(nm, fmt) ': nmfmts) : bts)
		('(a, fmt, nm, b) ': sais) where
	bindingAndArrayElemImage b a = (b, a)

instance BindingAndArrayElemImage bts sais =>
	BindingAndArrayElemImage ('DescriptorSetLayout.Image '[] : bts)
	sais where
	bindingAndArrayElemImage b _ =
		bindingAndArrayElemImage @bts @sais (b + 1) 0

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage
		('DescriptorSetLayout.Image nmfmts : bts) sais =>
	BindingAndArrayElemImage
		('DescriptorSetLayout.Image (nmfmt ': nmfmts) ': bts) sais where
	bindingAndArrayElemImage b a = bindingAndArrayElemImage
		@('DescriptorSetLayout.Image nmfmts : bts) @sais b (a + 1)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemImage bts sais =>
	BindingAndArrayElemImage (bt ': bts) sais where
	bindingAndArrayElemImage b _ = bindingAndArrayElemImage @bts @sais (b + 1) 0
