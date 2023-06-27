{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.TypeLevel.Common where

import GHC.TypeLits
import Control.Arrow
import Data.Kind
import Data.Kind.Object qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout

type LayoutArg = (Type, [Layout.BindingType])

type family LayoutArgOnlyDynamics la where
	LayoutArgOnlyDynamics '(_t, bts) =
		Layout.BindingTypeListBufferOnlyDynamics bts

type family LayoutArgListOnlyDynamics las where
	LayoutArgListOnlyDynamics '[] = '[]
	LayoutArgListOnlyDynamics (la ': las) =
		LayoutArgOnlyDynamics la ': LayoutArgListOnlyDynamics las

type family BindingTypesFromLayoutArg (tbts :: LayoutArg) ::
	[DescriptorSetLayout.BindingType] where
	BindingTypesFromLayoutArg '(t, bts) = bts

class IsPrefix (objs :: [VObj.Object]) (objs' :: [VObj.Object]) where
	isPrefixUpdateDynamicLength ::
		HeteroParList.PL KObj.ObjectLength (VObj.OnlyDynamics objs') ->
		HeteroParList.PL KObj.ObjectLength (VObj.OnlyDynamics objs) ->
		HeteroParList.PL KObj.ObjectLength (VObj.OnlyDynamics objs')

instance IsPrefix '[] objs where isPrefixUpdateDynamicLength lns _ = lns

instance IsPrefix os os' =>
	IsPrefix (VObj.Static o : os) (VObj.Static o : os') where
	isPrefixUpdateDynamicLength lns' lns  = isPrefixUpdateDynamicLength @os @os' lns' lns

instance IsPrefix os os' =>
	IsPrefix (VObj.Dynamic n o : os) (VObj.Dynamic n o : os') where
	isPrefixUpdateDynamicLength (_ln :** lns') (ln :** lns) =
		ln :** isPrefixUpdateDynamicLength @os @os' lns' lns

class BindingAndArrayElem
	(bts :: [DescriptorSetLayout.BindingType]) (objs :: [VObj.Object]) (i :: Nat) where
	bindingAndArrayElem :: Integral n => n -> (n, n)
	updateDynamicLength ::
		HeteroParList.PL
			(HeteroParList.PL KObj.ObjectLength)
			(Layout.BindingTypeListBufferOnlyDynamics bts) ->
		HeteroParList.PL KObj.ObjectLength
			(VObj.OnlyDynamics objs) ->
		HeteroParList.PL
			(HeteroParList.PL KObj.ObjectLength)
			(Layout.BindingTypeListBufferOnlyDynamics bts)

instance BindingAndArrayElem _bts '[] i where
	bindingAndArrayElem _ = error "badbadbad"
	updateDynamicLength a _ = a

instance IsPrefix os os' =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer (VObj.Atom algn t 'Nothing ': os') ': bts)
		(VObj.Atom algn t ('Just nm) ': os) 0 where
	bindingAndArrayElem _ = (0, 0)
	updateDynamicLength (lns' :** lnss) lns =
		isPrefixUpdateDynamicLength @os @os' lns' lns :** lnss

instance IsPrefix os os' =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer (VObj.Atom algn t ('Just nm) ': os') ': bts)
		(VObj.Atom algn t 'Nothing ': os) 0 where
	bindingAndArrayElem _ = (0, 0)
	updateDynamicLength (lns' :** lnss) lns =
		isPrefixUpdateDynamicLength @os @os' lns' lns :** lnss

{-
instance IsPrefix os os' =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer (VObj.DynAtom n algn t 'Nothing ': os') ': bts)
		(VObj.DynAtom n algn t ('Just nm) ': os) where
	bindingAndArrayElem _ = (0, 0)
	updateDynamicLength (lns' :** lnss) (ln :** lns) =
		(ln :** isPrefixUpdateDynamicLength @os @os' lns' lns) :** lnss

instance IsPrefix os os' =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer (VObj.DynAtom n algn t ('Just nm) ': os') ': bts)
		(VObj.DynAtom n algn t 'Nothing ': os) where
	bindingAndArrayElem _ = (0, 0)
	updateDynamicLength (lns' :** lnss) (ln :** lns) =
		(ln :** isPrefixUpdateDynamicLength @os @os' lns' lns) :** lnss
-}

instance {-# OVERLAPPABLE #-} IsPrefix os os' =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer (VObj.Static o ': os') ': bts)
		(VObj.Static o ': os) 0 where
	bindingAndArrayElem _ = (0, 0)
	updateDynamicLength (lns' :** lnss) lns =
		isPrefixUpdateDynamicLength @os @os' lns' lns :** lnss

instance {-# OVERLAPPABLE #-} IsPrefix os os' =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer (VObj.Dynamic n o ': os') ': bts)
		(VObj.Dynamic n o ': os) 0 where
	bindingAndArrayElem _ = (0, 0)
	updateDynamicLength ((_ln :** lns') :** lnss) (ln :** lns) =
		(ln :** isPrefixUpdateDynamicLength @os @os' lns' lns) :** lnss

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer os' ': bts) (oo ': os) (i - 1) =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer (oo ': os') ': bts) (oo ': os) i where
	bindingAndArrayElem c = (+ 1) `second`
		(bindingAndArrayElem
			@('DescriptorSetLayout.Buffer os' ': bts) @(oo ': os) @(i - 1) $ c + 1)
	updateDynamicLength _lnss _lns = error "not implemented"

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer os' ': bts) (oo ': os) i =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer (VObj.Static o ': os') ': bts) (oo ': os) i where
	bindingAndArrayElem c = (+ 1) `second`
		(bindingAndArrayElem
			@('DescriptorSetLayout.Buffer os' ': bts) @(oo ': os) @i $ c + 1)
	updateDynamicLength lnss lns =
		updateDynamicLength
			@('DescriptorSetLayout.Buffer os' ': bts) @(oo ': os) @i lnss lns

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer os' ': bts) (oo ': os) i =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer (VObj.Dynamic n o ': os') ': bts) (oo ': os) i where
	bindingAndArrayElem c = (+ 1) `second`
		(bindingAndArrayElem
			@('DescriptorSetLayout.Buffer os' ': bts) @(oo ': os) @i $ c + 1)
	updateDynamicLength ((ln :** lns') :** lnss) lns = let
		ls' :** lss = updateDynamicLength
			@('DescriptorSetLayout.Buffer os' ': bts) @(oo ': os) @i (lns' :** lnss) lns in
		(ln :** ls') :** lss

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem bts (oo ': os) i =>
	BindingAndArrayElem
		('DescriptorSetLayout.Buffer '[] ': bts) (oo ': os) i where
	bindingAndArrayElem c = (a + 1, b - c) where (a, b) = bindingAndArrayElem @bts @(oo ': os) @i 0
	updateDynamicLength (HeteroParList.Nil :** lnss) lns =
		HeteroParList.Nil :** updateDynamicLength @bts @(oo ': os) @i lnss lns

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElem bts (oo ': os) i =>
	BindingAndArrayElem
		('DescriptorSetLayout.Other ': bts) (oo ': os) i where
	bindingAndArrayElem c = (a + 1, b - c)
		where (a, b) = bindingAndArrayElem @bts @(oo ': os) @i 0
	updateDynamicLength (HeteroParList.Nil :** lnss) lns =
		HeteroParList.Nil :** updateDynamicLength @bts @(oo ': os) @i lnss lns

{-
type SampleBts0 = '[
	'DescriptorSetLayout.Buffer '[ ],
	'DescriptorSetLayout.Buffer '[ 'N.Atom 256 Double 'Nothing, 'N.List 256 () "", 'N.Atom 256 Bool 'Nothing],
	'DescriptorSetLayout.Other,
	'DescriptorSetLayout.Buffer
		'[ 'N.Atom 256 Bool 'Nothing, 'N.List 256 () "", 'N.Atom 256 Int 'Nothing, 'N.List 256 Double "", 'N.Atom 256 Double 'Nothing]]

type SampleObjs0 = '[ 'N.Atom 256 Int 'Nothing, 'N.List 256 Double ""]

type SampleBts1 = '[
	'DescriptorSetLayout.Buffer '[ 'N.List 256 Double "", 'N.List 256 Double "", 'N.List 256 Double ""],
	'DescriptorSetLayout.Buffer '[ 'N.Atom 256 Double 'Nothing ] ]

type SampleObj1 = '[ 'N.Atom 256 Double 'Nothing]
-}

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
	BindingAndArrayElemImage ('DescriptorSetLayout.Image '[] ': bts)
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

class IsPrefixBufferView (bvs :: [(Symbol, Type)]) (bvs' :: [(Symbol, Type)])

instance IsPrefixBufferView '[] _bvs'

instance IsPrefixBufferView bvs bvs' =>
	IsPrefixBufferView (bv ': bvs) (bv ': bvs')

class BindingAndArrayElemBufferView
	(bt :: [DescriptorSetLayout.BindingType])
	(bvs :: [(Symbol, Type)]) (i :: Nat) where
	bindingAndArrayElemBufferView :: Integral n => n -> n -> (n, n)

instance IsPrefixBufferView bvs bvs' =>
	BindingAndArrayElemBufferView
		('DescriptorSetLayout.BufferView ('(nm, t) ': bvs') ': bts)
		('(nm, t) ': bvs) 0 where
	bindingAndArrayElemBufferView b a = (b, a)

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemBufferView
		('DescriptorSetLayout.BufferView bvs' ': bts)
		('(nm, t) ': bvs) (i - 1) =>
	BindingAndArrayElemBufferView
		('DescriptorSetLayout.BufferView ('(nm, t) ': bvs') ': bts)
		('(nm, t) ': bvs) i where
	bindingAndArrayElemBufferView b a = bindingAndArrayElemBufferView
		@('DescriptorSetLayout.BufferView bvs' ': bts)
		@('(nm, t) ': bvs) @(i - 1) b a

instance {-# OVERLAPPABLE #-} BindingAndArrayElemBufferView
	('DescriptorSetLayout.BufferView bvs' ': bts) bvs i =>
	BindingAndArrayElemBufferView
		('DescriptorSetLayout.BufferView (nmt ': bvs') ': bts) bvs i where
	bindingAndArrayElemBufferView b a = bindingAndArrayElemBufferView
		@('DescriptorSetLayout.BufferView bvs' ': bts) @bvs @i b (a + 1)

instance {-# OVERLAPPABLE #-} BindingAndArrayElemBufferView bts bvs i =>
	BindingAndArrayElemBufferView
--	('DescriptorSetLayout.BufferView '[] ': bts) bvs where
		(bt ': bts) bvs i where
	bindingAndArrayElemBufferView b _a =
		bindingAndArrayElemBufferView @bts @bvs @i (b + 1) 0
