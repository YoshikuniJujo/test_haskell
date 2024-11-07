{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PhysicalDevice (

	-- * ENUMERATE

	enumerate, M.P,

	-- * PHYSICAL DEVICE PROPERTIES AND FEATURES

	M.getProperties, M.Properties(..), M.getFeatures,

	-- ** Get Properties 2

	getProperties2ExtensionName,
	M.getFeatures2, M.Features2(..),

	-- * OTHER PROPERTIES

	M.getMemoryProperties, M.MemoryProperties(..),
	M.getQueueFamilyProperties, M.getFormatProperties,
	enumerateExtensionProperties,
	ExtensionProperties(..), ExtensionName(..),

	-- * OTHER FEATURES

	M.ShaderDrawParametersFeatures(..),

	-- * EXTENSIONS

	maintenance3ExtensionName,

	-- * ENUM AND STRUCT

	module Gpu.Vulkan.PhysicalDevice.Enum,

	Limits(..), Features(..),
	DescriptorIndexingFeatures(..), DescriptorIndexingFeaturesNoNext(..)

	) where

import Gpu.Vulkan.Internal
import Gpu.Vulkan.Instance.Internal qualified as Instance
import Gpu.Vulkan.Instance.Type qualified as Instance.T
import Gpu.Vulkan.PhysicalDevice.Middle qualified as M
import Gpu.Vulkan.PhysicalDevice.Enum
import Gpu.Vulkan.PhysicalDevice.Struct

import Gpu.Vulkan.Middle qualified as M
import Data.Text qualified as T

enumerate :: Instance.T.I s -> IO [M.P]
enumerate (Instance.T.I i) = M.enumerate i

enumerateExtensionProperties ::
	M.P -> Maybe LayerName -> IO [ExtensionProperties]
enumerateExtensionProperties p (((\(LayerName ln) -> ln) <$>) -> mlnm) =
	(extensionPropertiesFromMiddle <$>)
		<$> M.enumerateExtensionProperties p mlnm

data ExtensionProperties = ExtensionProperties {
	extensionPropertiesExtensionName :: ExtensionName,
	extensionPropertiesSpecVersion :: M.ApiVersion }
	deriving Show

extensionPropertiesFromMiddle :: M.ExtensionProperties -> ExtensionProperties
extensionPropertiesFromMiddle M.ExtensionProperties {
	M.extensionPropertiesExtensionName = en,
	M.extensionPropertiesSpecVersion = sv } = ExtensionProperties {
	extensionPropertiesExtensionName = ExtensionName en,
	extensionPropertiesSpecVersion = sv }

newtype ExtensionName = ExtensionName { unExtensionName :: T.Text }
	deriving (Show, Eq)

maintenance3ExtensionName :: ExtensionName
maintenance3ExtensionName = ExtensionName M.maintenance3ExtensionName

getProperties2ExtensionName :: Instance.ExtensionName
getProperties2ExtensionName = Instance.ExtensionName M.getProperties2ExtensionName
