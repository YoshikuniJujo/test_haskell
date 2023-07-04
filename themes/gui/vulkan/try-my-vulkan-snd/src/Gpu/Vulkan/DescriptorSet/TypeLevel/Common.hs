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
import Data.TypeLevel.List qualified as TList
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Gpu.Vulkan.Object qualified as VObj
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout

import qualified Gpu.Vulkan.TypeEnum as T

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

type family LayoutArgListOnlyDynamics las where
	LayoutArgListOnlyDynamics '[] = '[]
	LayoutArgListOnlyDynamics (la ': las) =
		Layout.BindingTypeListBufferOnlyDynamics (TIndex.I1_2 la) ':
			LayoutArgListOnlyDynamics las

class VObj.OnlyDynamicLengths objs =>
	BindingAndArrayElemFoo
	(bts :: [Layout.BindingType]) (objs :: [VObj.Object]) where
	updateDynamicLength ::
		HeteroParList.PL
			(HeteroParList.PL KObj.ObjectLength)
			(Layout.BindingTypeListBufferOnlyDynamics bts) ->
		HeteroParList.PL KObj.ObjectLength
			(VObj.OnlyDynamics objs) ->
		HeteroParList.PL
			(HeteroParList.PL KObj.ObjectLength)
			(Layout.BindingTypeListBufferOnlyDynamics bts)

instance BindingAndArrayElemFoo _bts '[] where updateDynamicLength a _ = a

instance (IsPrefix os os', VObj.OnlyDynamicLengths os) =>
	BindingAndArrayElemFoo
		('Layout.Buffer (VObj.Atom algn t 'Nothing ': os') ': bts)
		(VObj.Atom algn t ('Just nm) ': os) where
	updateDynamicLength (lns' :** lnss) lns =
		isPrefixUpdateDynamicLength @os @os' lns' lns :** lnss

instance (IsPrefix os os', VObj.OnlyDynamicLengths os) =>
	BindingAndArrayElemFoo
		('Layout.Buffer (VObj.Atom algn t ('Just nm) ': os') ': bts)
		(VObj.Atom algn t 'Nothing ': os) where
	updateDynamicLength (lns' :** lnss) lns =
		isPrefixUpdateDynamicLength @os @os' lns' lns :** lnss

instance {-# OVERLAPPABLE #-} (IsPrefix os os', VObj.OnlyDynamicLengths os) =>
	BindingAndArrayElemFoo
		('Layout.Buffer (VObj.Static o ': os') ': bts)
		(VObj.Static o ': os) where
	updateDynamicLength (lns' :** lnss) lns =
		isPrefixUpdateDynamicLength @os @os' lns' lns :** lnss

instance {-# OVERLAPPABLE #-} (IsPrefix os os', VObj.OnlyDynamicLengths os) =>
	BindingAndArrayElemFoo
		('Layout.Buffer (VObj.Dynamic n o ': os') ': bts)
		(VObj.Dynamic n o ': os) where
	updateDynamicLength ((_ln :** lns') :** lnss) (ln :** lns) =
		(ln :** isPrefixUpdateDynamicLength @os @os' lns' lns) :** lnss

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemFoo
		('Layout.Buffer os' ': bts) (oo ': os) =>
	BindingAndArrayElemFoo
		('Layout.Buffer (VObj.Static o ': os') ': bts) (oo ': os) where
	updateDynamicLength lnss lns =
		updateDynamicLength
			@('Layout.Buffer os' ': bts) @(oo ': os) lnss lns

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemFoo
		('Layout.Buffer os' ': bts) (oo ': os) =>
	BindingAndArrayElemFoo
		('Layout.Buffer (VObj.Dynamic n o ': os') ': bts) (oo ': os) where
	updateDynamicLength ((ln :** lns') :** lnss) lns = let
		ls' :** lss = updateDynamicLength
			@('Layout.Buffer os' ': bts) @(oo ': os) (lns' :** lnss) lns in
		(ln :** ls') :** lss

instance {-# OVERLAPPABLE #-}
	BindingAndArrayElemFoo bts (oo ': os) =>
	BindingAndArrayElemFoo
		('Layout.Buffer '[] ': bts) (oo ': os) where
	updateDynamicLength (HeteroParList.Nil :** lnss) lns =
		HeteroParList.Nil :** updateDynamicLength @bts @(oo ': os) lnss lns

class IsPrefixImage
	(sais :: [(Type, T.Format, Symbol, Type)])
	(btis :: [(Symbol, T.Format)])

instance IsPrefixImage '[] bit

instance IsPrefixImage sais btis =>
	IsPrefixImage ('(a, fmt, nm, b) ': sais) ('(nm, fmt) ': btis)

class BindingAndArrayElemImage
	(bts :: [Layout.BindingType])
	(imgs :: [(Type, T.Format, Symbol, Type)]) where
	bindingAndArrayElemImage :: Integral n => n -> n -> (n, n)

instance IsPrefixImage sais nmfmts =>
	BindingAndArrayElemImage
		('Layout.Image ('(nm, fmt) ': nmfmts) : bts)
		('(a, fmt, nm, b) ': sais) where
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

class IsPrefixBufferView (bvs :: [(Symbol, Type)]) (bvs' :: [(Symbol, Type)])

instance IsPrefixBufferView '[] _bvs'

instance IsPrefixBufferView bvs bvs' =>
	IsPrefixBufferView (bv ': bvs) (bv ': bvs')

class BindingAndArrayElemBufferView
	(bt :: [Layout.BindingType])
	(bvs :: [(Symbol, Type)]) (i :: Nat) where
	bindingAndArrayElemBufferView :: Integral n => n -> n -> (n, n)

instance IsPrefixBufferView bvs bvs' =>
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
