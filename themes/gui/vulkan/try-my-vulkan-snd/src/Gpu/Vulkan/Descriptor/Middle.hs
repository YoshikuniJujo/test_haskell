{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor.Middle (

	-- * ImageInfo and BufferInfo

	ImageInfo(..), BufferInfo(..),

	-- * Type

	Type,
	pattern TypeSampler, pattern TypeCombinedImageSampler,
	pattern TypeSampledImage, pattern TypeStorageImage,
	pattern TypeUniformTexelBuffer, pattern TypeStorageTexelBuffer,
	pattern TypeUniformBuffer, pattern TypeStorageBuffer,
	pattern TypeUniformBufferDynamic, pattern TypeStorageBufferDynamic,
	pattern TypeInputAttachment, pattern TypeInlineUniformBlock,
	pattern TypeAccelerationStructureKhr,
	pattern TypeAccelerationStructureNv, pattern TypeMutableValve,
	pattern TypeSampleWeightImageQcom, pattern TypeBlockMatchImageQcom,
	pattern TypeInlineUniformBlockExt, pattern TypeMaxEnum

	) where

import Gpu.Vulkan.Descriptor.Middle.Internal
import Gpu.Vulkan.Descriptor.Enum
