{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Middle (

	-- * ENUMERATE, PROPERTIES AND FEATURES

	enumerate, P, getProperties, Properties(..), getFeatures,

	-- ** Get Properties 2

	getProperties2ExtensionName,
	getFeatures2', Features2(..),

	-- * OTHER PROPERTIES

	enumerateExtensionProperties,
	getQueueFamilyProperties,
	getFormatProperties,
	getMemoryProperties, MemoryProperties(..),

	-- * OTHER FEATURES

	ShaderDrawParametersFeatures(..),

	-- * OTHER EXTENSIONS

	maintenance3ExtensionName

	) where

import Gpu.Vulkan.PhysicalDevice.Middle.Internal
