{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor.Middle.Internal (
	extensionNameIndexing,
	ImageInfo(..), imageInfoToCore, BufferInfo(..), bufferInfoToCore ) where

import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer
import qualified Gpu.Vulkan.Image.Enum as Image
import qualified Gpu.Vulkan.ImageView.Middle.Internal as ImageView
import qualified Gpu.Vulkan.Sampler.Middle.Internal as Sampler
import qualified Gpu.Vulkan.Descriptor.Core as C

import Data.Text qualified as T

#include <vulkan/vulkan.h>

extensionNameIndexing :: T.Text
extensionNameIndexing = #{const_str VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME}

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

imageInfoToCore :: ImageInfo -> IO C.ImageInfo
imageInfoToCore ImageInfo {
	imageInfoSampler = Sampler.S s,
	imageInfoImageView = iv,
	imageInfoImageLayout = Image.Layout l } = do
	iv' <- ImageView.iToCore iv
	pure C.ImageInfo {
		C.imageInfoSampler = s,
		C.imageInfoImageView = iv',
		C.imageInfoImageLayout = l }
