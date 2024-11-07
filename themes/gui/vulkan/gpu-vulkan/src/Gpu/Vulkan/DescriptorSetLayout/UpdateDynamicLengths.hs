{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.UpdateDynamicLengths (

	-- * UPDATE DYNAMIC LENGTHS

	UpdateDynamicLength(..) ) where

import Gpu.Vulkan.Object qualified as VObj
import Gpu.Vulkan.Object.Base qualified as KObj
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout

class VObj.OnlyDynamicLengths objs => UpdateDynamicLength bts objs where
	updateDynamicLength ::
		HeteroParList.PL
			(HeteroParList.PL KObj.Length)
			(Layout.BindingTypeListBufferOnlyDynamics bts) ->
		HeteroParList.PL KObj.Length
			(VObj.OnlyDynamics objs) ->
		HeteroParList.PL
			(HeteroParList.PL KObj.Length)
			(Layout.BindingTypeListBufferOnlyDynamics bts)

instance UpdateDynamicLength _bts '[] where updateDynamicLength x _ = x

instance UpdateDynamicLength bts (o ': os) =>
	UpdateDynamicLength ('Layout.Buffer '[] ': bts) (o ': os) where
	updateDynamicLength (HeteroParList.Nil :** lnss) lns =
		HeteroParList.Nil :** updateDynamicLength @bts @(o ': os) lnss lns

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.Static algn 'Nothing ot t ': os') ': bts)
		(VObj.Static algn ('Just _nm) ot t ': os) where
	updateDynamicLength (lns' :** lnss) lns =
		updateDynamicLengthPrefix @os @os' lns' lns :** lnss

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.Static algn ('Just _nm) ot t ': os') ': bts)
		(VObj.Static algn 'Nothing ot t ': os) where
	updateDynamicLength (lns' :** lnss) lns =
		updateDynamicLengthPrefix @os @os' lns' lns :** lnss

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.Dynamic n algn 'Nothing ot t ': os') ': bts)
		(VObj.Dynamic n algn ('Just _nm) ot t ': os) where
	updateDynamicLength ((_ln :** lns') :** lnss) (ln :** lns) =
		(KObj.renameLength ln :** updateDynamicLengthPrefix @os @os' lns' lns) :** lnss

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.Dynamic n algn ('Just _nm) ot t ': os') ': bts)
		(VObj.Dynamic n algn 'Nothing ot t ': os) where
	updateDynamicLength ((_ln :** lns') :** lnss) (ln :** lns) =
		(KObj.renameLength ln :** updateDynamicLengthPrefix @os @os' lns' lns) :** lnss

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.Static_ o ': os') ': bts)
		(VObj.Static_ o ': os) where
	updateDynamicLength (lns' :** lnss) lns =
		updateDynamicLengthPrefix @os @os' lns' lns :** lnss

instance (UpdateDynamicLengthPrefix os os', VObj.OnlyDynamicLengths os) =>
	UpdateDynamicLength
		('Layout.Buffer ('VObj.Dynamic n o ': os') ': bts)
		('VObj.Dynamic n o ': os) where
	updateDynamicLength ((_ln :** lns') :** lnss) (ln :** lns) =
		(ln :** updateDynamicLengthPrefix @os @os' lns' lns) :** lnss

instance {-# OVERLAPPABLE #-}
	UpdateDynamicLength
		('Layout.Buffer os' ': bts) (oo ': os) =>
	UpdateDynamicLength
		('Layout.Buffer (VObj.Static_ o ': os') ': bts) (oo ': os) where
	updateDynamicLength lnss lns =
		updateDynamicLength
			@('Layout.Buffer os' ': bts) @(oo ': os) lnss lns

instance {-# OVERLAPPABLE #-}
	UpdateDynamicLength
		('Layout.Buffer os' ': bts) (oo ': os) =>
	UpdateDynamicLength
		('Layout.Buffer ('VObj.Dynamic n o ': os') ': bts) (oo ': os) where
	updateDynamicLength ((ln :** lns') :** lnss) lns = let
		ls' :** lss = updateDynamicLength
			@('Layout.Buffer os' ': bts) @(oo ': os) (lns' :** lnss) lns in
		(ln :** ls') :** lss

class UpdateDynamicLengthPrefix (objs :: [VObj.O]) (objs' :: [VObj.O]) where
	updateDynamicLengthPrefix ::
		HeteroParList.PL KObj.Length (VObj.OnlyDynamics objs') ->
		HeteroParList.PL KObj.Length (VObj.OnlyDynamics objs) ->
		HeteroParList.PL KObj.Length (VObj.OnlyDynamics objs')

instance UpdateDynamicLengthPrefix '[] objs where
	updateDynamicLengthPrefix lns _ = lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.Static algn 'Nothing ot t ': os)
		(VObj.Static algn ('Just _nm) ot t ': os') where
	updateDynamicLengthPrefix lns' lns  =
		updateDynamicLengthPrefix @os @os' lns' lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.Static algn ('Just _nm) ot t ': os)
		(VObj.Static algn 'Nothing ot t ': os') where
	updateDynamicLengthPrefix lns' lns  =
		updateDynamicLengthPrefix @os @os' lns' lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.Dynamic n algn 'Nothing ot t ': os)
		(VObj.Dynamic n algn ('Just _nm) ot t ': os') where
	updateDynamicLengthPrefix (_ln :** lns') (ln :** lns)  =
		KObj.renameLength ln :** updateDynamicLengthPrefix @os @os' lns' lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.Dynamic n algn ('Just _nm) ot t ': os)
		(VObj.Dynamic n algn 'Nothing ot t ': os') where
	updateDynamicLengthPrefix (_ln :** lns') (ln :** lns)  =
		KObj.renameLength ln :** updateDynamicLengthPrefix @os @os' lns' lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		(VObj.Static_ o : os) (VObj.Static_ o : os') where
	updateDynamicLengthPrefix lns' lns  =
		updateDynamicLengthPrefix @os @os' lns' lns

instance UpdateDynamicLengthPrefix os os' =>
	UpdateDynamicLengthPrefix
		('VObj.Dynamic n o : os) ('VObj.Dynamic n o : os') where
	updateDynamicLengthPrefix (_ln :** lns') (ln :** lns) =
		ln :** updateDynamicLengthPrefix @os @os' lns' lns
