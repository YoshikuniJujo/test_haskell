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
	WriteSourcesArg(..),
	WriteSourcesToMiddle(..), WriteSourcesToLengthList(..) ) where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList.Tuple qualified as HeteroParList
import Data.Word

import Gpu.Vulkan.Object qualified as VObj

import Gpu.Vulkan.TypeEnum qualified as T

import Gpu.Vulkan.Descriptor.Internal qualified as Descriptor
import Gpu.Vulkan.Descriptor.Middle qualified as Descriptor.M

import Gpu.Vulkan.DescriptorSet.TypeLevel.Common
import Gpu.Vulkan.DescriptorSet.Middle qualified as M

import Gpu.Vulkan.Buffer.Type qualified as Buffer
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
		HeteroParList.PL (U3 BufferView.B) texelBufferViewsArgs ->
		WriteSources ('WriteSourcesArgBufferView texelBufferViewsArgs)
	WriteSourcesInNext :: DstBinding -> DstArrayElement ->
		DescriptorCount -> WriteSources 'WriteSourcesArgInNext

type DstBinding = Word32
type DstArrayElement = Word32
type DescriptorCount = Word32

class (
	BindingAndArrayElem (TIndex.I1_2 slbts) (WriteSourcesToLengthListObj wsarg) 0,
	WriteSourcesToLengthList wsarg ) =>
	WriteSourcesToMiddle (slbts :: LayoutArg) wsarg where
	writeSourcesToMiddle ::
		WriteSources wsarg -> ((Word32, Word32), M.WriteSources)

instance (
	HeteroParList.Map3_4 smsbnmobjs,
	BindingAndArrayElem
		(TIndex.I1_2 slbts)
		(TMapIndex.M3_4 smsbnmobjs) 0,
	BufferInfoListToMiddleNew smsbnmobjs ) =>
	WriteSourcesToMiddle slbts ('WriteSourcesArgBuffer smsbnmobjs) where
	writeSourcesToMiddle (BufferInfos bis) = (
		bindingAndArrayElem
			@(TIndex.I1_2 slbts)
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
	| WriteSourcesArgBuffer [(Type, Type, Symbol, VObj.Object)]
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

class WriteSourcesToLengthList arg where
	type WriteSourcesToLengthListObj arg :: [VObj.Object]
	writeSourcesToLengthList :: WriteSources arg ->
		Maybe (HeteroParList.PL
			VObj.ObjectLength (WriteSourcesToLengthListObj arg))

instance
	HeteroParList.Map3_4 sbsmobjsobjs =>
	WriteSourcesToLengthList ('WriteSourcesArgBuffer sbsmobjsobjs) where
	type WriteSourcesToLengthListObj
		('WriteSourcesArgBuffer sbsmobjsobjs) =
		TMapIndex.M3_4 sbsmobjsobjs
	writeSourcesToLengthList (BufferInfos bis) =
		Just $ HeteroParList.map3_4 (toLength . unU4) bis
		where
		toLength :: Descriptor.BufferInfo sm sb nm obj ->
			VObj.ObjectLength obj
		toLength (Descriptor.BufferInfo (Buffer.Binded lns _)) =
			HeteroParList.typeIndex lns

instance WriteSourcesToLengthList ('WriteSourcesArgImage ssfmtnmsis) where
	type WriteSourcesToLengthListObj
		('WriteSourcesArgImage ssfmtnmsis) = '[]
	writeSourcesToLengthList (ImageInfos _bis) = Nothing

instance WriteSourcesToLengthList ('WriteSourcesArgBufferView foo) where
	type WriteSourcesToLengthListObj
		('WriteSourcesArgBufferView foo) = '[]
	writeSourcesToLengthList (TexelBufferViews _) = Nothing

instance WriteSourcesToLengthList 'WriteSourcesArgInNext where
	type WriteSourcesToLengthListObj 'WriteSourcesArgInNext = '[]
	writeSourcesToLengthList (WriteSourcesInNext _ _ _) = Nothing
