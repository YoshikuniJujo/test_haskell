{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Write.Sources (

	-- * WRITE SOURCES

	-- ** Types

	WriteSources(..), WriteSourcesArg(..),
	DstBinding, DstArrayElement, DescriptorCount,

	-- ** WriteSourcesToMiddle

	WriteSourcesToMiddle(..),
	BindingAndArrayElemImage, BindingAndArrayElemImageWithImmutableSampler,
	BindingAndArrayElemBuffer, BindingAndArrayElemBufferView,

	-- ** WriteSourcesUpdateDynamicLengths

	WriteSourcesUpdateDynamicLengths(..), UpdateDynamicLength

	) where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
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
import Gpu.Vulkan.DescriptorSet.BindingAndArrayElem.Buffer
import Gpu.Vulkan.DescriptorSet.Middle qualified as M
import Gpu.Vulkan.DescriptorSetLayout.Type qualified as Layout
import Gpu.Vulkan.DescriptorSetLayout.UpdateDynamicLengths

import Gpu.Vulkan.Buffer.Type qualified as Buffer
import Gpu.Vulkan.BufferView.Internal qualified as BufferView
import Gpu.Vulkan.BufferView.Middle qualified as BufferView.M

-- * WRITE SOURCES

-- ** Types

data WriteSources arg where
	ImageInfos ::
		HeteroParList.PL (U4 Descriptor.ImageInfo) iiargs ->
		WriteSources ('WriteSourcesArgImage iiargs)
	ImageInfosNoSampler ::
		HeteroParList.PL (U3 Descriptor.ImageInfoNoSampler) iiargs ->
		WriteSources ('WriteSourcesArgImageNoSampler iiargs)
	BufferInfos ::
		HeteroParList.PL (U5 Descriptor.BufferInfo) biargs ->
		WriteSources ('WriteSourcesArgBuffer biargs)
	TexelBufferViews ::
		HeteroParList.PL (U3 BufferView.B) tbvargs ->
		WriteSources ('WriteSourcesArgBufferView tbvargs)
	WriteSourcesInNext :: DstBinding -> DstArrayElement ->
		DescriptorCount -> WriteSources 'WriteSourcesArgInNext

data WriteSourcesArg
	= WriteSourcesArgImage [(Type, Symbol, T.Format, Type)]
	| WriteSourcesArgImageNoSampler [(Symbol, T.Format, Type)]
	| WriteSourcesArgBuffer [(Type, Type, Symbol, VObj.O, Nat)]
	| WriteSourcesArgBufferView [(Type, Symbol, Type)]
	| WriteSourcesArgInNext

type DstBinding = Word32
type DstArrayElement = Word32
type DescriptorCount = Word32

-- ** WriteSourcesToMiddle

class WriteSourcesToMiddle (lbts :: [Layout.BindingType]) wsarg (i :: Nat) where
	writeSourcesToMiddle ::
		WriteSources wsarg -> ((Word32, Word32), M.WriteSources)

instance (BindingAndArrayElemBuffer lbts (TMapIndex.M3_5 barg) i) =>
	WriteSourcesToMiddle lbts ('WriteSourcesArgBuffer barg) i where
	writeSourcesToMiddle (BufferInfos bis) = (
		bindingAndArrayElemBuffer @lbts @(TMapIndex.M3_5 barg) @i 0 0,
		M.WriteSourcesBufferInfo $ bufferInfoListToMiddle bis )
		where
		bufferInfoListToMiddle ::
			HeteroParList.PL (U5 Descriptor.BufferInfo) biargs ->
			[Descriptor.M.BufferInfo]
		bufferInfoListToMiddle = HeteroParList.toList \(U5 bi) ->
			Descriptor.bufferInfoToMiddle bi

instance BindingAndArrayElemImage lbts (TMapIndex.M1'2_4 iarg) i =>
	WriteSourcesToMiddle lbts ('WriteSourcesArgImage iarg) i where
	writeSourcesToMiddle (ImageInfos iis) = (
		bindingAndArrayElemImage @lbts @(TMapIndex.M1'2_4 iarg) @i 0 0,
		M.WriteSourcesImageInfo $ imageInfosToMiddle iis )
		where
		imageInfosToMiddle ::
			HeteroParList.PL (U4 Descriptor.ImageInfo) iiargs ->
			[Descriptor.M.ImageInfo]
		imageInfosToMiddle = HeteroParList.toList \(U4 ii) ->
			Descriptor.imageInfoToMiddle ii

instance BindingAndArrayElemImageWithImmutableSampler
		lbts (TMapIndex.M0'1_3 iarg) i =>
	WriteSourcesToMiddle lbts ('WriteSourcesArgImageNoSampler iarg) i where
	writeSourcesToMiddle (ImageInfosNoSampler iis) = (
		bindingAndArrayElemImageWithImmutableSampler
			@lbts @(TMapIndex.M0'1_3 iarg) @i 0 0,
		M.WriteSourcesImageInfo $ imageInfosToMiddle iis )
		where
		imageInfosToMiddle ::
			HeteroParList.PL
				(U3 Descriptor.ImageInfoNoSampler) iiargs ->
			[Descriptor.M.ImageInfo]
		imageInfosToMiddle = HeteroParList.toList \(U3 ii) ->
			Descriptor.imageInfoNoSamplerToMiddle ii

instance BindingAndArrayElemBufferView lbts (TMapIndex.M1'2_3 bvarg) i =>
	WriteSourcesToMiddle lbts ('WriteSourcesArgBufferView bvarg) i where
	writeSourcesToMiddle (TexelBufferViews bvarg) = (
		bindingAndArrayElemBufferView
			@lbts @(TMapIndex.M1'2_3 bvarg) @i 0 0,
		M.WriteSourcesBufferView $ bvsToMiddle bvarg )
		where
		bvsToMiddle :: HeteroParList.PL (U3 BufferView.B) bvs ->
			[BufferView.M.B]
		bvsToMiddle =
			HeteroParList.toList \(U3 (BufferView.B mbv)) -> mbv

instance WriteSourcesToMiddle lbts 'WriteSourcesArgInNext i where
	writeSourcesToMiddle (WriteSourcesInNext bdg ae cnt) =
		((bdg, ae), M.WriteSourcesInNext cnt)

-- ** WriteSourcesUpdateDynamicLengths

class WriteSourcesUpdateDynamicLengths lbts wsarg where
	writeSourcesUpdateDynamicLength ::
		D sds '(sl, lbts) -> WriteSources wsarg -> IO ()

instance (
	TMapIndex.M3_5 bargs ~ objs, UpdateDynamicLength lbts objs,
	HeteroParList.Map3_5 bargs ) =>
	WriteSourcesUpdateDynamicLengths
		lbts (WriteSourcesArgBuffer bargs) where
	writeSourcesUpdateDynamicLength (D rlns _) (BufferInfos bis) = do
		lns <- readIORef rlns
		writeIORef rlns
			. updateDynamicLength @lbts @objs lns
			. VObj.onlyDynamicLength @objs
			$ HeteroParList.map3_5 (toLength . unU5) bis
		where
		toLength :: Descriptor.BufferInfo sm sb nm obj i ->
			VObj.Length obj
		toLength (Descriptor.BufferInfo (Buffer.Binded lns _)) =
			HeteroParList.typeIndex lns

instance {-# OVERLAPPABLE #-} WriteSourcesUpdateDynamicLengths lbts wsarg where
	writeSourcesUpdateDynamicLength _ _ = pure ()
