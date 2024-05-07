{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor.Internal (

	-- * EXTENSION NAME

	indexingExtensionName,

	-- * BUFFER INFO

	BufferInfo(..), bufferInfoToMiddle,

	-- * IMAGE INFO

	ImageInfo(..), imageInfoToMiddle,
	ImageInfoNoSampler(..), imageInfoNoSamplerToMiddle

	) where

import Gpu.Vulkan.Object qualified as VObj

import qualified Gpu.Vulkan.Buffer as Buffer
import qualified Gpu.Vulkan.Buffer.Type as Buffer
import qualified Gpu.Vulkan.Descriptor.Middle as M

import qualified Gpu.Vulkan.Sampler.Type as Sampler
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.ImageView as ImageView
import qualified Gpu.Vulkan.ImageView.Type as ImageView

import Gpu.Vulkan.Sampler.Middle as Sampler.M

import Gpu.Vulkan.PhysicalDevice qualified as PhysicalDevice

indexingExtensionName :: PhysicalDevice.ExtensionName
indexingExtensionName = PhysicalDevice.ExtensionName M.indexingExtensionName

data BufferInfo sm sb nm obj i = forall objs .
	(Show (Buffer.Binded sm sb nm objs), VObj.OffsetRange obj objs i) =>
	BufferInfo (Buffer.Binded sm sb nm objs)

deriving instance Show (BufferInfo sm sb nm obj i)

bufferInfoToMiddle :: forall sb sm nm obj i .
	BufferInfo sm sb nm obj i -> M.BufferInfo
bufferInfoToMiddle (BufferInfo (Buffer.Binded lns b)) = M.BufferInfo {
	M.bufferInfoBuffer = b,
	M.bufferInfoOffset = ost,
	M.bufferInfoRange = rng }
	where (ost, rng) = VObj.offsetRange @obj @_ @i 0 lns

data ImageInfo ss fmt nm si = ImageInfo {
	imageInfoSampler :: Sampler.S ss,
	imageInfoImageView :: ImageView.I fmt nm si,
	imageInfoImageLayout :: Image.Layout }
	deriving Show

imageInfoToMiddle ::
	ImageInfo ss fmt nm si -> M.ImageInfo
imageInfoToMiddle ImageInfo {
	imageInfoSampler = s,
	imageInfoImageView = ImageView.I iv,
	imageInfoImageLayout = lyt } = M.ImageInfo {
	M.imageInfoSampler = Sampler.sToMiddle s,
	M.imageInfoImageView = iv,
	M.imageInfoImageLayout = lyt }

data ImageInfoNoSampler fmt nm si = ImageInfoNoSampler {
	imageInfoNoSamplerImageView :: ImageView.I fmt nm si,
	imageInfoNoSamplerImageLayout :: Image.Layout }
	deriving Show

imageInfoNoSamplerToMiddle ::
	ImageInfoNoSampler fmt nm si -> M.ImageInfo
imageInfoNoSamplerToMiddle ImageInfoNoSampler {
	imageInfoNoSamplerImageView = ImageView.I iv,
	imageInfoNoSamplerImageLayout = lyt } = M.ImageInfo {
	M.imageInfoSampler = Sampler.M.Null,
	M.imageInfoImageView = iv,
	M.imageInfoImageLayout = lyt }
