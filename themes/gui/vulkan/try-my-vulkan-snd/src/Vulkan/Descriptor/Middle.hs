{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Middle where

import Foreign.Storable

import qualified Foreign.Storable.Generic

import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Buffer.Middle as Buffer
import qualified Vulkan.Image.Enum as Image
import qualified Vulkan.ImageView.Middle as ImageView
import qualified Vulkan.Sampler as Sampler
import qualified Vulkan.Descriptor.Core as C

data BufferInfo = BufferInfo {
	bufferInfoBuffer :: Buffer.B,
	bufferInfoOffset :: Device.Size,
	bufferInfoRange :: Device.Size }
	deriving Show

bufferInfoToCore :: BufferInfo -> C.BufferInfo
bufferInfoToCore BufferInfo {
	bufferInfoBuffer = Buffer.B b,
	bufferInfoOffset = Device.Size os,
	bufferInfoRange = Device.Size rg } = C.BufferInfo {
	C.bufferInfoBuffer = b,
	C.bufferInfoOffset = os,
	C.bufferInfoRange = rg }

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
