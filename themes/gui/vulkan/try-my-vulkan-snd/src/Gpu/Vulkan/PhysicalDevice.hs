{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice (

	-- * ENUMERATE

	enumerate, M.P,

	-- * FEATURES

	M.getFeatures,

	-- * PROPERTIES

	M.getProperties, M.Properties(..),
	M.getMemoryProperties, M.MemoryProperties(..),
	M.getQueueFamilyProperties, M.getFormatProperties,
	M.enumerateExtensionProperties,

	) where

import Gpu.Vulkan.Instance.Type qualified as Instance.T
import Gpu.Vulkan.PhysicalDevice.Middle qualified as M

enumerate :: Instance.T.I s -> IO [M.P]
enumerate (Instance.T.I i) = M.enumerate i
