{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor.Internal (

	BufferInfoNew(..), BufferInfoArgNew, bufferInfoToMiddleNew,
	BufferInfoListToLengthNew(..),

	ImageInfo(..), imageInfoToMiddle,

	) where

import GHC.TypeLits
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Gpu.Vulkan.Object qualified as VObj
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Descriptor.Middle as M

import qualified Gpu.Vulkan.Sampler as Sampler
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.ImageView as ImageView

data BufferInfoNew sm sb nm obj =
	forall objs . (Show (Buffer.Binded sm sb nm objs), VObj.OffsetRange obj objs, VObj.ObjectLengthIndex obj objs) =>
		BufferInfoNew (Buffer.Binded sm sb nm objs)

deriving instance Show (BufferInfoNew sm sb nm obj)

type BufferInfoArgNew = (Type, Type, Symbol, VObj.Object)

bufferInfoToLengthNew ::
	BufferInfoNew sb sm nm obj -> VObj.ObjectLength obj
bufferInfoToLengthNew (BufferInfoNew (Buffer.Binded lns _)) =
	VObj.objectLengthIndex lns

class BufferInfoListToLengthNew sbsmobjsobjs where
	type BufferInfoListToLengthObjsNew sbsmobjsobjs :: [VObj.Object]
	bufferInfoListToLengthNew ::
		HeteroParList.PL (U4 BufferInfoNew) sbsmobjsobjs ->
		HeteroParList.PL VObj.ObjectLength
			(BufferInfoListToLengthObjsNew sbsmobjsobjs)

instance BufferInfoListToLengthNew '[] where
	type BufferInfoListToLengthObjsNew '[] = '[]
	bufferInfoListToLengthNew HeteroParList.Nil = HeteroParList.Nil

instance (
	BufferInfoListToLengthNew sbsmobjsobjs ) =>
	BufferInfoListToLengthNew ('(sb, sm, nm, obj) ': sbsmobjsobjs) where
	type BufferInfoListToLengthObjsNew ('(sb, sm, nm, obj) ': sbsmobjsobjs) =
		obj ': BufferInfoListToLengthObjsNew sbsmobjsobjs
	bufferInfoListToLengthNew (U4 bi :** bis) =
		bufferInfoToLengthNew bi :** bufferInfoListToLengthNew bis

bufferInfoToMiddleNew :: forall sb sm nm obj .
	BufferInfoNew sm sb nm obj -> M.BufferInfo
bufferInfoToMiddleNew (BufferInfoNew (Buffer.Binded lns b)) = M.BufferInfo {
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
