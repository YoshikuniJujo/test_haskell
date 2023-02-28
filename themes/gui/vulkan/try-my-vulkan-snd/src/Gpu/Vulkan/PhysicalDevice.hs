{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice (
	M.P, enumerate,
	M.getFeatures, M.getQueueFamilyProperties,
	M.enumerateExtensionProperties,
	M.getFormatProperties,

	M.Properties(..), M.getProperties,
	M.MemoryProperties(..), M.getMemoryProperties,
	) where

import Gpu.Vulkan.Instance.Type qualified as Instance.T
import Gpu.Vulkan.PhysicalDevice.Middle qualified as M

enumerate :: Instance.T.I s -> IO [M.P]
enumerate (Instance.T.I i) = M.enumerate i
