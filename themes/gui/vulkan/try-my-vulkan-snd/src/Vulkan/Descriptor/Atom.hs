{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Atom where

import Foreign.Storable

import qualified Foreign.Storable.Generic

import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Buffer.Atom as Buffer.A
import qualified Vulkan.Image.Enum as Image
import qualified Vulkan.ImageView.Middle as ImageView
import qualified Vulkan.Sampler as Sampler
import qualified Vulkan.Descriptor.Core as C

data BufferInfo v = BufferInfo {
	bufferInfoBuffer :: Buffer.A.B v,
	bufferInfoOffset :: Device.Size }
	deriving Show

bufferInfoToCore :: forall v . Storable (Foreign.Storable.Generic.Wrap v) =>
	BufferInfo v -> C.BufferInfo
bufferInfoToCore BufferInfo {
	bufferInfoBuffer = Buffer.A.B b,
	bufferInfoOffset = Device.Size sz } = C.BufferInfo {
	C.bufferInfoBuffer = b,
	C.bufferInfoOffset = sz,
	C.bufferInfoRange = fromIntegral
		$ sizeOf @(Foreign.Storable.Generic.Wrap v) undefined }

data ImageInfo = ImageInfo {
	imageInfoSampler :: Sampler.S,
	imageInfoImageView :: ImageView.I,
	imageInfoImageLayout :: Image.Layout }
	deriving Show

imageInfoToCore :: ImageInfo -> C.ImageInfo
imageInfoToCore ImageInfo {
	imageInfoSampler = Sampler.S s,
	imageInfoImageView = ImageView.I i,
	imageInfoImageLayout = Image.Layout l } = C.ImageInfo {
	C.imageInfoSampler = s,
	C.imageInfoImageView = i,
	C.imageInfoImageLayout = l }
