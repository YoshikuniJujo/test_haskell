{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Write.Sources (
	WriteSources(..), DstBinding, DstArrayElement, DescriptorCount,
	WriteSourcesArg(..), WriteSourcesToMiddle(..) ) where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.TypeEnum qualified as T

import Gpu.Vulkan.Descriptor.Internal qualified as Descriptor
import Gpu.Vulkan.Descriptor.Middle qualified as Descriptor.M

import Gpu.Vulkan.DescriptorSet.TypeLevel.Common
import Gpu.Vulkan.DescriptorSet.Middle qualified as M

import Gpu.Vulkan.BufferView.Internal qualified as BufferView
import Gpu.Vulkan.BufferView.Middle qualified as BufferView.M

data WriteSources arg where
	ImageInfos ::
		HeteroParList.PL (U4 Descriptor.ImageInfo) imageInfoArgs ->
		WriteSources ('WriteSourcesArgImage imageInfoArgs)
	BufferInfos ::
		HeteroParList.PL (U4 Descriptor.BufferInfo) bufferInfoArgs ->
		WriteSources ('WriteSourcesArgBuffer bufferInfoArgs)
	TexelBufferViews ::
		HeteroParList.PL (U3 BufferView.B) nmts ->
		WriteSources ('WriteSourcesArgBufferView nmts)
	WriteSourcesInNext :: DstBinding -> DstArrayElement ->
		DescriptorCount -> WriteSources 'WriteSourcesArgInNext

type DstBinding = Word32
type DstArrayElement = Word32
type DescriptorCount = Word32

class WriteSourcesToMiddle (slbts :: LayoutArg) wsarg where
	writeSourcesToMiddle ::
		WriteSources wsarg -> ((Word32, Word32), M.WriteSources)

instance (
	BindingAndArrayElem
		(BindingTypesFromLayoutArg slbts)
		(TMapIndex.M3_4 smsbnmobjs) 0,
	BufferInfoListToMiddleNew smsbnmobjs ) =>
	WriteSourcesToMiddle slbts ('WriteSourcesArgBuffer smsbnmobjs) where
	writeSourcesToMiddle (BufferInfos bis) = (
		bindingAndArrayElem
			@(BindingTypesFromLayoutArg slbts)
			@(TMapIndex.M3_4 smsbnmobjs) @0 0,
		M.WriteSourcesBufferInfo $ bufferInfoListToMiddleNew bis )

class BufferInfoListToMiddleNew smsbnmobjs where
	bufferInfoListToMiddleNew ::
		HeteroParList.PL (U4 Descriptor.BufferInfo) smsbnmobjs ->
		[Descriptor.M.BufferInfo]

instance BufferInfoListToMiddleNew '[] where
	bufferInfoListToMiddleNew HeteroParList.Nil = []

instance BufferInfoListToMiddleNew smsbnmobjs =>
	BufferInfoListToMiddleNew ('(sm, sb, nm, obj) ': smsbnmobjs) where
	bufferInfoListToMiddleNew (U4 bi :** bis) =
		Descriptor.bufferInfoToMiddle bi :
		bufferInfoListToMiddleNew bis

instance (
	BindingAndArrayElemImage bts ssfmtnmsis,
	ImageInfosToMiddle ssfmtnmsis ) =>
	WriteSourcesToMiddle '(sl, bts) ('WriteSourcesArgImage ssfmtnmsis) where
	writeSourcesToMiddle (ImageInfos iis) = (
		bindingAndArrayElemImage @bts @ssfmtnmsis 0 0,
		M.WriteSourcesImageInfo $ imageInfosToMiddle iis )

instance (
	BindingAndArrayElemBufferView bts (TMapIndex.M1'2_3 bvs) 0,
	BufferViewsToMiddle bvs ) =>
	WriteSourcesToMiddle '(sl, bts) ('WriteSourcesArgBufferView bvs) where
	writeSourcesToMiddle (TexelBufferViews bvs) = (
		bindingAndArrayElemBufferView @bts @(TMapIndex.M1'2_3 bvs) @0 0 0,
		M.WriteSourcesBufferView $ bufferViewsToMiddle bvs )

instance WriteSourcesToMiddle slbts 'WriteSourcesArgInNext where
	writeSourcesToMiddle = \case
		WriteSourcesInNext bdg ae cnt -> ((bdg, ae), M.WriteSourcesInNext cnt)

data WriteSourcesArg
	= WriteSourcesArgImage [(Type, T.Format, Symbol, Type)]
	| WriteSourcesArgBuffer [Descriptor.BufferInfoArgs]
	| WriteSourcesArgBufferView [(Type, Symbol, Type)]
	| WriteSourcesArgInNext

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
		HeteroParList.PL (U3 BufferView.B) bvs -> [BufferView.M.B]

instance BufferViewsToMiddle '[] where
	bufferViewsToMiddle HeteroParList.Nil = []

instance BufferViewsToMiddle bvs =>
	BufferViewsToMiddle (bv ': bvs) where
	bufferViewsToMiddle (U3 (BufferView.B mbv) :** bvs) =
		mbv : bufferViewsToMiddle bvs
