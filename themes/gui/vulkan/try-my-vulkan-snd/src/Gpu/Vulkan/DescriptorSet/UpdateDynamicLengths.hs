{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.UpdateDynamicLengths where

import Data.Kind.Object qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout

class VObj.OnlyDynamicLengths objs => UpdateDynamicLength bts objs where
	updateDynamicLength ::
		HeteroParList.PL
			(HeteroParList.PL KObj.ObjectLength)
			(Layout.BindingTypeListBufferOnlyDynamics bts) ->
		HeteroParList.PL KObj.ObjectLength
			(VObj.OnlyDynamics objs) ->
		HeteroParList.PL
			(HeteroParList.PL KObj.ObjectLength)
			(Layout.BindingTypeListBufferOnlyDynamics bts)

instance UpdateDynamicLength _bts '[] where updateDynamicLength x _ = x

instance UpdateDynamicLength bts (o ': os) =>
	UpdateDynamicLength ('Layout.Buffer '[] ': bts) (o ': os) where
	updateDynamicLength (HeteroParList.Nil :** lnss) lns =
		HeteroParList.Nil :** updateDynamicLength @bts @(o ': os) lnss lns

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.StaticObject algn 'Nothing ot t ': os') ': bts)
		(VObj.StaticObject algn ('Just _nm) ot t ': os) where
	updateDynamicLength (lns' :** lnss) lns =
		updateDynamicLengthPrefix @os @os' lns' lns :** lnss

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.StaticObject algn ('Just _nm) ot t ': os') ': bts)
		(VObj.StaticObject algn 'Nothing ot t ': os) where
	updateDynamicLength (lns' :** lnss) lns =
		updateDynamicLengthPrefix @os @os' lns' lns :** lnss

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.DynamicObject n algn 'Nothing ot t ': os') ': bts)
		(VObj.DynamicObject n algn ('Just _nm) ot t ': os) where
	updateDynamicLength ((_ln :** lns') :** lnss) (ln :** lns) =
		(KObj.adjustDynamicLength ln :** updateDynamicLengthPrefix @os @os' lns' lns) :** lnss

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.DynamicObject n algn ('Just _nm) ot t ': os') ': bts)
		(VObj.DynamicObject n algn 'Nothing ot t ': os) where
	updateDynamicLength ((_ln :** lns') :** lnss) (ln :** lns) =
		(KObj.adjustDynamicLength ln :** updateDynamicLengthPrefix @os @os' lns' lns) :** lnss

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.Static o ': os') ': bts)
		(VObj.Static o ': os) where
	updateDynamicLength (lns' :** lnss) lns =
		updateDynamicLengthPrefix @os @os' lns' lns :** lnss

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.Dynamic n o ': os') ': bts)
		(VObj.Dynamic n o ': os) where
	updateDynamicLength ((_ln :** lns') :** lnss) (ln :** lns) =
		(ln :** updateDynamicLengthPrefix @os @os' lns' lns) :** lnss

instance {-# OVERLAPPABLE #-}
	UpdateDynamicLength
		('Layout.Buffer os' ': bts) (oo ': os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.Static o ': os') ': bts) (oo ': os) where
	updateDynamicLength lnss lns =
		updateDynamicLength
			@('Layout.Buffer os' ': bts) @(oo ': os) lnss lns

instance {-# OVERLAPPABLE #-}
	UpdateDynamicLength
		('Layout.Buffer os' ': bts) (oo ': os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.Dynamic n o ': os') ': bts) (oo ': os) where
	updateDynamicLength ((ln :** lns') :** lnss) lns = let
		ls' :** lss = updateDynamicLength
			@('Layout.Buffer os' ': bts) @(oo ': os) (lns' :** lnss) lns in
		(ln :** ls') :** lss

class UpdateDynamicLengthPrefix (objs :: [VObj.Object]) (objs' :: [VObj.Object]) where
	updateDynamicLengthPrefix ::
		HeteroParList.PL KObj.ObjectLength (VObj.OnlyDynamics objs') ->
		HeteroParList.PL KObj.ObjectLength (VObj.OnlyDynamics objs) ->
		HeteroParList.PL KObj.ObjectLength (VObj.OnlyDynamics objs')

instance UpdateDynamicLengthPrefix '[] objs where
	updateDynamicLengthPrefix lns _ = lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.StaticObject algn 'Nothing ot t ': os)
		(VObj.StaticObject algn ('Just _nm) ot t ': os') where
	updateDynamicLengthPrefix lns' lns  =
		updateDynamicLengthPrefix @os @os' lns' lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.StaticObject algn ('Just _nm) ot t ': os)
		(VObj.StaticObject algn 'Nothing ot t ': os') where
	updateDynamicLengthPrefix lns' lns  =
		updateDynamicLengthPrefix @os @os' lns' lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.DynamicObject n algn 'Nothing ot t ': os)
		(VObj.DynamicObject n algn ('Just _nm) ot t ': os') where
	updateDynamicLengthPrefix (_ln :** lns') (ln :** lns)  =
		KObj.adjustDynamicLength ln :** updateDynamicLengthPrefix @os @os' lns' lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.DynamicObject n algn ('Just _nm) ot t ': os)
		(VObj.DynamicObject n algn 'Nothing ot t ': os') where
	updateDynamicLengthPrefix (_ln :** lns') (ln :** lns)  =
		KObj.adjustDynamicLength ln :** updateDynamicLengthPrefix @os @os' lns' lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.Static o : os) (VObj.Static o : os') where
	updateDynamicLengthPrefix lns' lns  =
		updateDynamicLengthPrefix @os @os' lns' lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.Dynamic n o : os) (VObj.Dynamic n o : os') where
	updateDynamicLengthPrefix (_ln :** lns') (ln :** lns) =
		ln :** updateDynamicLengthPrefix @os @os' lns' lns
