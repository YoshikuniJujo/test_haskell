{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Format where

import Vulkan.Enum

import qualified Vulkan.Format.Core as C

data FormatProperties = FormatProperties {
	formatPropertiesLinearTilingFeatures :: FormatFeatureFlags,
	formatPropertiesOptimalTilingFeatures :: FormatFeatureFlags,
	formatPropertiesBufferFeatures :: FormatFeatureFlags }
	deriving Show

propertiesFromCore :: C.FormatProperties -> FormatProperties
propertiesFromCore C.FormatProperties {
	C.formatPropertiesLinearTilingFeatures = ltfs,
	C.formatPropertiesOptimalTilingFeatures = otfs,
	C.formatPropertiesBufferFeatures = bfs
	} = FormatProperties {
		formatPropertiesLinearTilingFeatures =
			FormatFeatureFlagBits ltfs,
		formatPropertiesOptimalTilingFeatures =
			FormatFeatureFlagBits otfs,
		formatPropertiesBufferFeatures = FormatFeatureFlagBits bfs }
