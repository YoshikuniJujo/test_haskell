{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice (

	-- * ENUMERATE

	enumerate, M.P,

	-- * GET FEATURES

	M.getFeatures,

	-- * PROPERTIES

	M.getProperties, M.Properties(..),
	M.getMemoryProperties, M.MemoryProperties(..),
	M.getQueueFamilyProperties, M.getFormatProperties,
	enumerateExtensionProperties,

	-- * FEATURES

	M.ShaderDrawParametersFeatures(..)

	) where

import Gpu.Vulkan.Internal
import Gpu.Vulkan.Instance.Type qualified as Instance.T
import Gpu.Vulkan.PhysicalDevice.Middle qualified as M

enumerate :: Instance.T.I s -> IO [M.P]
enumerate (Instance.T.I i) = M.enumerate i

enumerateExtensionProperties ::
	M.P -> Maybe LayerName -> IO [ExtensionProperties]
enumerateExtensionProperties p (((\(LayerName ln) -> ln) <$>) -> mlnm) =
	(extensionPropertiesFromMiddle <$>)
		<$> M.enumerateExtensionProperties p mlnm
