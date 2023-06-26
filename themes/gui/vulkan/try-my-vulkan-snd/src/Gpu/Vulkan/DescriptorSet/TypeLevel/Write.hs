{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.TypeLevel.Write (
	module Gpu.Vulkan.DescriptorSet.TypeLevel.Write,
	module Gpu.Vulkan.DescriptorSet.TypeLevel.Common
	) where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.TypeEnum qualified as T
import Gpu.Vulkan.Object qualified as VObj

import Gpu.Vulkan.Descriptor.Internal qualified as Descriptor
import Gpu.Vulkan.Descriptor.Middle qualified as Descriptor.M

import Gpu.Vulkan.DescriptorSet.TypeLevel.Common
import Gpu.Vulkan.DescriptorSet.Middle qualified as M

import Gpu.Vulkan.BufferView.Internal qualified as BufferView
import Gpu.Vulkan.BufferView.Middle qualified as BufferView.M

class WriteSourcesToMiddle (slbts :: LayoutArg) wsarg where
	type WriteSourcesObjs wsarg :: [VObj.Object]
	writeSourcesToMiddle ::
		WriteSources wsarg -> ((Word32, Word32), M.WriteSources)

instance (
	BindingAndArrayElem
		(BindingTypesFromLayoutArg slbts)
		(TMapIndex.M3_4 smsbnmobjs) 0,
	BufferInfoListToMiddleNew smsbnmobjs ) =>
	WriteSourcesToMiddle slbts ('WriteSourcesArgBufferNew smsbnmobjs) where
	type WriteSourcesObjs ('WriteSourcesArgBufferNew smsbnmobjs) =
		TMapIndex.M3_4 smsbnmobjs
	writeSourcesToMiddle (BufferInfosNew bis) = (
		bindingAndArrayElem
			@(BindingTypesFromLayoutArg slbts)
			@(TMapIndex.M3_4 smsbnmobjs) @0 0,
		M.WriteSourcesBufferInfo $ bufferInfoListToMiddleNew bis )

instance (
	BindingAndArrayElemImage bts ssfmtnmsis,
	ImageInfosToMiddle ssfmtnmsis ) =>
	WriteSourcesToMiddle '(sl, bts) ('WriteSourcesArgImage ssfmtnmsis) where
	type WriteSourcesObjs ('WriteSourcesArgImage ssfmtnmsis) = '[]
	writeSourcesToMiddle (ImageInfos iis) = (
		bindingAndArrayElemImage @bts @ssfmtnmsis 0 0,
		M.WriteSourcesImageInfo $ imageInfosToMiddle iis )

instance (
	BindingAndArrayElemBufferView bts bvs 0,
	BufferViewsToMiddle bvs ) =>
	WriteSourcesToMiddle '(sl, bts) ('WriteSourcesArgBufferView bvs) where
	type WriteSourcesObjs ('WriteSourcesArgBufferView bvs) = '[]
	writeSourcesToMiddle (TexelBufferViews bvs) = (
		bindingAndArrayElemBufferView @bts @bvs @0 0 0,
		M.WriteSourcesBufferView $ bufferViewsToMiddle bvs )

instance WriteSourcesToMiddle slbts 'WriteSourcesArgOther where
	type WriteSourcesObjs 'WriteSourcesArgOther = '[]
	writeSourcesToMiddle = \case
		WriteSourcesInNext bdg ae cnt -> ((bdg, ae), M.WriteSourcesInNext cnt)
		TexelBufferViewsOld bdg ae bvs -> ((bdg, ae), M.WriteSourcesBufferView bvs)

data WriteSources arg where
	WriteSourcesInNext ::
		Word32 -> Word32 -> Word32 -> WriteSources 'WriteSourcesArgOther
	ImageInfos ::
		HeteroParList.PL (U4 Descriptor.ImageInfo) ssfmtnmsis ->
		WriteSources ('WriteSourcesArgImage ssfmtnmsis)
	BufferInfosNew ::
		HeteroParList.PL (U4 Descriptor.BufferInfoNew) smsbnmobjs ->
		WriteSources ('WriteSourcesArgBufferNew smsbnmobjs)
	TexelBufferViews ::
		HeteroParList.PL (U2 (BufferView.B sb)) nmts ->
		WriteSources ('WriteSourcesArgBufferView nmts)
	TexelBufferViewsOld ::
		Word32 -> Word32 -> [BufferView.M.B] ->
		WriteSources 'WriteSourcesArgOther

class BufferInfoListToMiddleNew smsbnmobjs where
	bufferInfoListToMiddleNew ::
		HeteroParList.PL (U4 Descriptor.BufferInfoNew) smsbnmobjs ->
		[Descriptor.M.BufferInfo]

instance BufferInfoListToMiddleNew '[] where
	bufferInfoListToMiddleNew HeteroParList.Nil = []

instance BufferInfoListToMiddleNew smsbnmobjs =>
	BufferInfoListToMiddleNew ('(sm, sb, nm, obj) ': smsbnmobjs) where
	bufferInfoListToMiddleNew (U4 bi :** bis) =
		Descriptor.bufferInfoToMiddleNew bi :
		bufferInfoListToMiddleNew bis

data WriteSourcesArg
	= WriteSourcesArgImage [(Type, T.Format, Symbol, Type)]
	| WriteSourcesArgBufferNew [Descriptor.BufferInfoArgNew]
	| WriteSourcesArgBufferView [(Symbol, Type)]
	| WriteSourcesArgOther

class ImageInfosToMiddle ssfmtnmsis where
	imageInfosToMiddle ::
		HeteroParList.PL (U4 Descriptor.ImageInfo) ssfmtnmsis ->
		[Descriptor.M.ImageInfo]

instance ImageInfosToMiddle '[] where imageInfosToMiddle HeteroParList.Nil = []

instance ImageInfosToMiddle ssfmtnmsis =>
	ImageInfosToMiddle ('(ss, fmt, nm, si) ': ssfmtnmsis) where
	imageInfosToMiddle (U4 ii :** iis) =
		Descriptor.imageInfoToMiddle ii : imageInfosToMiddle iis

class BufferViewsToMiddle bvs where
	bufferViewsToMiddle ::
		HeteroParList.PL (U2 (BufferView.B s)) bvs -> [BufferView.M.B]

instance BufferViewsToMiddle '[] where
	bufferViewsToMiddle HeteroParList.Nil = []

instance BufferViewsToMiddle bvs =>
	BufferViewsToMiddle (bv ': bvs) where
	bufferViewsToMiddle (U2 (BufferView.B mbv) :** bvs) =
		mbv : bufferViewsToMiddle bvs
