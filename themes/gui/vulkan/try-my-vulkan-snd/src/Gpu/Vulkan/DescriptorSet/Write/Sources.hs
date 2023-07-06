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
	WriteSourcesToMiddle(..),

	BufferInfoListToMiddle,

	WriteSourcesUpdateDynamicLengths(..),

	) where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.HeteroParList (pattern (:**))
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList.Tuple qualified as HeteroParList
import Data.Word
import Data.IORef

import Gpu.Vulkan.Object qualified as VObj

import Gpu.Vulkan.TypeEnum qualified as T

import Gpu.Vulkan.Descriptor.Internal qualified as Descriptor
import Gpu.Vulkan.Descriptor.Middle qualified as Descriptor.M

import Gpu.Vulkan.DescriptorSet.Type
import Gpu.Vulkan.DescriptorSet.BindingAndArrayElem
import Gpu.Vulkan.DescriptorSet.Middle qualified as M
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout
import Gpu.Vulkan.DescriptorSetLayout.UpdateDynamicLengths

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

class WriteSourcesToMiddle (bts :: [Layout.BindingType]) wsarg where
	writeSourcesToMiddle ::
		WriteSources wsarg -> ((Word32, Word32), M.WriteSources)

instance (
	BindingAndArrayElem bts (TMapIndex.M3_4 wsbarg) 0,
	BufferInfoListToMiddle wsbarg ) =>
	WriteSourcesToMiddle bts ('WriteSourcesArgBuffer wsbarg) where
	writeSourcesToMiddle (BufferInfos bis) = (
		bindingAndArrayElem @bts @(TMapIndex.M3_4 wsbarg) @0 0 0,
		M.WriteSourcesBufferInfo $ bufferInfoListToMiddle bis )

class BufferInfoListToMiddle smsbnmobjs where
	bufferInfoListToMiddle ::
		HeteroParList.PL (U4 Descriptor.BufferInfo) smsbnmobjs ->
		[Descriptor.M.BufferInfo]

instance BufferInfoListToMiddle '[] where
	bufferInfoListToMiddle HeteroParList.Nil = []

instance BufferInfoListToMiddle smsbnmobjs =>
	BufferInfoListToMiddle ('(sm, sb, nm, obj) ': smsbnmobjs) where
	bufferInfoListToMiddle (U4 bi :** bis) =
		Descriptor.bufferInfoToMiddle bi :
		bufferInfoListToMiddle bis

instance (
	BindingAndArrayElemImage bts ssfmtnmsis,
	ImageInfosToMiddle ssfmtnmsis ) =>
	WriteSourcesToMiddle bts ('WriteSourcesArgImage ssfmtnmsis) where
	writeSourcesToMiddle (ImageInfos iis) = (
		bindingAndArrayElemImage @bts @ssfmtnmsis 0 0,
		M.WriteSourcesImageInfo $ imageInfosToMiddle iis )

instance (
	BindingAndArrayElemBufferView bts (TMapIndex.M1'2_3 bvs) 0,
	BufferViewsToMiddle bvs ) =>
	WriteSourcesToMiddle bts ('WriteSourcesArgBufferView bvs) where
	writeSourcesToMiddle (TexelBufferViews bvs) = (
		bindingAndArrayElemBufferView
			@bts @(TMapIndex.M1'2_3 bvs) @0 0 0,
		M.WriteSourcesBufferView $ bufferViewsToMiddle bvs )

instance WriteSourcesToMiddle bts 'WriteSourcesArgInNext where
	writeSourcesToMiddle (WriteSourcesInNext bdg ae cnt) =
		((bdg, ae), M.WriteSourcesInNext cnt)

data WriteSourcesArg
	= WriteSourcesArgImage [(Type, Symbol, T.Format, Type)]
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

writeSourcesUpdateDynamicLengths :: HeteroParList.Map3_4 ss =>
	HeteroParList.PL (U4 Descriptor.BufferInfo) ss ->
	HeteroParList.PL VObj.ObjectLength (TMapIndex.M3_4 ss)
writeSourcesUpdateDynamicLengths bis =
	HeteroParList.map3_4 (toLength . unU4) bis
	where
	toLength :: Descriptor.BufferInfo sm sb nm obj ->
		VObj.ObjectLength obj
	toLength (Descriptor.BufferInfo (Buffer.Binded lns _)) =
		HeteroParList.typeIndex lns

class WriteSourcesUpdateDynamicLengths bts arg where
	writeSourcesUpdateDynamicLength :: D sds '(sl, bts) -> WriteSources arg -> IO ()

instance (
	UpdateDynamicLength bts (TMapIndex.M3_4 foo),
	HeteroParList.Map3_4 foo ) =>
	WriteSourcesUpdateDynamicLengths bts (WriteSourcesArgBuffer foo) where
	writeSourcesUpdateDynamicLength (D rlns _) (BufferInfos bis) = do
		lns <- readIORef rlns
		(writeIORef rlns . updateDynamicLength @bts @(TMapIndex.M3_4 foo) lns
			. (VObj.onlyDynamicLength @(TMapIndex.M3_4 foo)))
			(writeSourcesUpdateDynamicLengths @foo bis)

instance {-# OVERLAPPABLE #-} WriteSourcesUpdateDynamicLengths bts foo where
	writeSourcesUpdateDynamicLength _ _ = pure ()
