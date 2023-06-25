{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor (

	BufferInfo(..), BufferInfoArg, ImageInfo(..),

	BufferInfoListToLength,

	BufferInfoNew(..), BufferInfoArgNew, bufferInfoToMiddleNew

	) where

import Gpu.Vulkan.Descriptor.Internal
