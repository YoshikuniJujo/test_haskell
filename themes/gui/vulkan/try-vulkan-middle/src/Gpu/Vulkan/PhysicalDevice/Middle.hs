{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice.Middle (

	-- * Type

	P,

	-- * Enumerate, Properties and Features

	enumerate, getProperties, Properties(..), getFeatures,

	-- * Other Properties

	enumerateExtensionProperties,
	getQueueFamilyProperties,
	getFormatProperties,
	getMemoryProperties, MemoryProperties(..),

	-- * Others

	ShaderDrawParametersFeatures(..)

	) where

import Gpu.Vulkan.PhysicalDevice.Middle.Internal
