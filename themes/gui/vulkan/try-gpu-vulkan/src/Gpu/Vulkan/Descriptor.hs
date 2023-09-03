{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor (

	-- * EXTENSION NAME

	extensionNameIndexing,

	-- * BUFFER INFO

	BufferInfo(..),

	-- * IMAGE INFO

	ImageInfo(..), ImageInfoNoSampler(..)

	) where

import Gpu.Vulkan.Descriptor.Internal
