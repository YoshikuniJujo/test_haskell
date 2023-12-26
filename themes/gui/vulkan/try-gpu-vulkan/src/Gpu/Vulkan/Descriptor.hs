{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor (

	-- * EXTENSION NAME

	indexingExtensionName,

	-- * BUFFER INFO

	BufferInfo(..),

	-- * IMAGE INFO

	ImageInfo(..), ImageInfoNoSampler(..),

	-- * ENUM

	module Gpu.Vulkan.Descriptor.Enum

	) where

import Gpu.Vulkan.Descriptor.Internal
import Gpu.Vulkan.Descriptor.Enum
