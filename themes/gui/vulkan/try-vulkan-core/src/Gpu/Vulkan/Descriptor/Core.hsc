{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor.Core (

	-- * BUFFER INFO

	BufferInfo, PtrBufferInfo,
	pattern BufferInfo,
	bufferInfoBuffer, bufferInfoOffset, bufferInfoRange,

	-- * IMAGE INFO

	ImageInfo, PtrImageInfo,
	pattern ImageInfo,
	imageInfoSampler, imageInfoImageView, imageInfoImageLayout

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import qualified Gpu.Vulkan.Buffer.Core as Buffer
import qualified Gpu.Vulkan.ImageView.Core as ImageView
import qualified Gpu.Vulkan.Sampler.Core as Sampler

#include <vulkan/vulkan.h>

struct "BufferInfo" #{size VkDescriptorBufferInfo}
		#{alignment VkDescriptorBufferInfo} [
	("buffer", ''Buffer.B,
		[| #{peek VkDescriptorBufferInfo, buffer} |],
		[| #{poke VkDescriptorBufferInfo, buffer} |]),
	("offset", ''#{type VkDeviceSize},
		[| #{peek VkDescriptorBufferInfo, offset} |],
		[| #{poke VkDescriptorBufferInfo, offset} |]),
	("range", ''#{type VkDeviceSize},
		[| #{peek VkDescriptorBufferInfo, range} |],
		[| #{poke VkDescriptorBufferInfo, range} |]) ]
	[''Show, ''Storable]

type PtrBufferInfo = Ptr BufferInfo

struct "ImageInfo" #{size VkDescriptorImageInfo}
		#{alignment VkDescriptorImageInfo} [
	("sampler", ''Sampler.S,
		[| #{peek VkDescriptorImageInfo, sampler} |],
		[| #{poke VkDescriptorImageInfo, sampler} |]),
	("imageView", ''ImageView.I,
		[| #{peek VkDescriptorImageInfo, imageView} |],
		[| #{poke VkDescriptorImageInfo, imageView} |]),
	("imageLayout", ''#{type VkImageLayout},
		[| #{peek VkDescriptorImageInfo, imageLayout} |],
		[| #{poke VkDescriptorImageInfo, imageLayout} |]) ]
	[''Show, ''Storable]

type PtrImageInfo = Ptr ImageInfo
