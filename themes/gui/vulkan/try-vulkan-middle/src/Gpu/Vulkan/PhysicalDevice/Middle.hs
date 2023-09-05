{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Middle (

	-- * ENUMERATE, PROPERTIES AND FEATURES

	enumerate, P, getProperties, Properties(..), getFeatures,

	getFeatures2, Features2Result(..),

	-- * OTHER PROPERTIES

	enumerateExtensionProperties,
	getQueueFamilyProperties,
	getFormatProperties,
	getMemoryProperties, MemoryProperties(..),

	-- * OTHER FEATURES

	ShaderDrawParametersFeatures(..)

	) where

import Gpu.Vulkan.PhysicalDevice.Middle.Internal
