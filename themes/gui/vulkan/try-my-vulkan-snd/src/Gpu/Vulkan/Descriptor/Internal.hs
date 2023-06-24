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

	BufferInfo(..), BufferInfoArg, bufferInfoToMiddle, ImageInfo(..), imageInfoToMiddle,

	BufferInfoListToLength(..),

	BufferInfoNew(..)

	) where

import GHC.TypeLits
import Data.Kind
import Gpu.Vulkan.Object qualified as VObj
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Descriptor.Middle as M

import qualified Gpu.Vulkan.Sampler as Sampler
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.ImageView as ImageView

data BufferInfo (sbsmobjsobj :: BufferInfoArg) where
	BufferInfoObj :: Buffer.Binded sm sb nm objs ->
		BufferInfo '(sb, sm, nm, objs, obj)

data BufferInfoNew sm sb nm obj =
	forall objs . (Show (Buffer.Binded sm sb nm objs), VObj.OffsetRange obj objs) =>
		BufferInfoNew (Buffer.Binded sm sb nm objs)

deriving instance Show (BufferInfoNew sm sb nm obj)

type BufferInfoArg = (Type, Type, Symbol, [VObj.Object], VObj.Object)

deriving instance Show (HeteroParList.PL VObj.ObjectLength objs) =>
	Show (BufferInfo '(sb, sm, nm, objs, obj))

bufferInfoToLength ::
	VObj.ObjectLengthIndex obj objs =>
	BufferInfo '(sb, sm, nm, objs, obj) -> VObj.ObjectLength obj
bufferInfoToLength (BufferInfoObj (Buffer.Binded lns _)) =
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

bufferInfoToMiddle :: forall sb sm nm objs obj . VObj.OffsetRange obj objs =>
	BufferInfo '(sb, sm, nm, objs, obj) -> M.BufferInfo
bufferInfoToMiddle (BufferInfoObj (Buffer.Binded lns b)) = M.BufferInfo {
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
