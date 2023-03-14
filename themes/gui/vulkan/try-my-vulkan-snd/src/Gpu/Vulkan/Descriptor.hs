{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor (

	BufferInfo(..), BufferInfoArg, bufferInfoToMiddle, ImageInfo(..), imageInfoToMiddle,

	BufferInfoListToLength(..),

	-- * Type

	E.Type,
	pattern E.TypeSampler, pattern E.TypeCombinedImageSampler,
	pattern E.TypeSampledImage, pattern E.TypeStorageImage,
	pattern E.TypeUniformTexelBuffer, pattern E.TypeStorageTexelBuffer,
	pattern E.TypeUniformBuffer, pattern E.TypeStorageBuffer,
	pattern E.TypeUniformBufferDynamic, pattern E.TypeStorageBufferDynamic,
	pattern E.TypeInputAttachment, pattern E.TypeInlineUniformBlock,
	pattern E.TypeAccelerationStructureKhr,
	pattern E.TypeAccelerationStructureNv, pattern E.TypeMutableValve,
	pattern E.TypeSampleWeightImageQcom, pattern E.TypeBlockMatchImageQcom,
	pattern E.TypeInlineUniformBlockExt, pattern E.TypeMaxEnum

	) where

import GHC.TypeLits
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Descriptor.Middle as M

import qualified Gpu.Vulkan.Sampler as Sampler
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.ImageView as ImageView

import Gpu.Vulkan.Descriptor.Enum qualified as E

data BufferInfo (sbsmobjsobj :: BufferInfoArg) where
	BufferInfoAtom ::
		{ bufferInfoAtomBuffer :: Buffer.Binded sm sb nm objs } ->
		BufferInfo '(sb, sm, nm, objs, VObj.Atom algn v objnm)
	BufferInfoList ::
		{ bufferInfoListBuffer :: Buffer.Binded sm sb nm objs } ->
		BufferInfo '(sb, sm, nm, objs, VObj.List algn v objnm)
	BufferInfoDynList ::
		{ bufferInfoDynListBuffer :: Buffer.Binded sm sb nm objs } ->
		BufferInfo '(sb, sm, nm, objs, VObj.DynList n algn v objnm)
	BufferInfoDynAtom ::
		{ bufferInfoDynAtomBuffer :: Buffer.Binded sm sb nm objs } ->
		BufferInfo '(sb, sm, nm, objs, VObj.DynAtom n algn v objnm)

type BufferInfoArg = (Type, Type, Symbol, [VObj.Object], VObj.Object)

deriving instance Show (HeteroParList.PL VObj.ObjectLength objs) =>
	Show (BufferInfo '(sb, sm, nm, objs, obj))

bufferInfoToLength ::
	VObj.ObjectLengthIndex obj objs =>
	BufferInfo '(sb, sm, nm, objs, obj) -> VObj.ObjectLength obj
bufferInfoToLength (BufferInfoAtom (Buffer.Binded lns _)) =
	VObj.objectLengthIndex lns
bufferInfoToLength (BufferInfoList (Buffer.Binded lns _)) =
	VObj.objectLengthIndex lns
bufferInfoToLength (BufferInfoDynAtom (Buffer.Binded lns _)) =
	VObj.objectLengthIndex lns
bufferInfoToLength (BufferInfoDynList (Buffer.Binded lns _)) =
	VObj.objectLengthIndex lns

class BufferInfoListToLength sbsmobjsobjs where
	type BufferInfoListToLengthObjs sbsmobjsobjs :: [VObj.Object]
	bufferInfoListToLength ::
		HeteroParList.PL BufferInfo sbsmobjsobjs ->
		HeteroParList.PL VObj.ObjectLength
			(BufferInfoListToLengthObjs sbsmobjsobjs)

instance BufferInfoListToLength '[] where
	type BufferInfoListToLengthObjs '[] = '[]
	bufferInfoListToLength HeteroParList.Nil = HeteroParList.Nil

instance (
	VObj.ObjectLengthIndex obj objs,
	BufferInfoListToLength sbsmobjsobjs ) =>
	BufferInfoListToLength ('(sb, sm, nm, objs, obj) ': sbsmobjsobjs) where
	type BufferInfoListToLengthObjs ('(sb, sm, nm, objs, obj) ': sbsmobjsobjs) =
		obj ': BufferInfoListToLengthObjs sbsmobjsobjs
	bufferInfoListToLength (bi :** bis) =
		bufferInfoToLength bi :** bufferInfoListToLength bis

bufferInfoToMiddle :: forall sb sm nm objs obj . VObj.Offset obj objs =>
	BufferInfo '(sb, sm, nm, objs, obj) -> M.BufferInfo
bufferInfoToMiddle BufferInfoAtom {
	bufferInfoAtomBuffer = Buffer.Binded lns b } = M.BufferInfo {
	M.bufferInfoBuffer = b,
	M.bufferInfoOffset = fromIntegral $ VObj.offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ VObj.range @obj lns }
bufferInfoToMiddle BufferInfoList {
	bufferInfoListBuffer = Buffer.Binded lns b } = M.BufferInfo {
	M.bufferInfoBuffer = b,
	M.bufferInfoOffset = fromIntegral $ VObj.offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ VObj.range @obj lns }
bufferInfoToMiddle BufferInfoDynList {
	bufferInfoDynListBuffer = Buffer.Binded lns b } = M.BufferInfo {
	M.bufferInfoBuffer = b,
	M.bufferInfoOffset = fromIntegral $ VObj.offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ VObj.range @obj lns }
bufferInfoToMiddle BufferInfoDynAtom {
	bufferInfoDynAtomBuffer = Buffer.Binded lns b } = M.BufferInfo {
	M.bufferInfoBuffer = b,
	M.bufferInfoOffset = fromIntegral $ VObj.offset @obj 0 lns,
	M.bufferInfoRange = fromIntegral $ VObj.range @obj lns }

data ImageInfo ss fmt nm si = ImageInfo {
	imageInfoSampler :: Sampler.S ss,
	imageInfoImageView :: ImageView.INew fmt nm si,
	imageInfoImageLayout :: Image.Layout }
	deriving Show

imageInfoToMiddle ::
	ImageInfo ss fmt nm si -> M.ImageInfo
imageInfoToMiddle ImageInfo {
	imageInfoSampler = s,
	imageInfoImageView = ImageView.INew iv,
	imageInfoImageLayout = lyt } = M.ImageInfo {
	M.imageInfoSampler = Sampler.sToMiddle s,
	M.imageInfoImageView = iv,
	M.imageInfoImageLayout = lyt }
