{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Format where

import Vulkan.Format.Enum

import qualified Vulkan.Format.Core as C

data Properties = Properties {
	propertiesLinearTilingFeatures :: FeatureFlags,
	propertiesOptimalTilingFeatures :: FeatureFlags,
	propertiesBufferFeatures :: FeatureFlags }
	deriving Show

propertiesFromCore :: C.Properties -> Properties
propertiesFromCore C.Properties {
	C.propertiesLinearTilingFeatures = ltfs,
	C.propertiesOptimalTilingFeatures = otfs,
	C.propertiesBufferFeatures = bfs
	} = Properties {
		propertiesLinearTilingFeatures = FeatureFlagBits ltfs,
		propertiesOptimalTilingFeatures = FeatureFlagBits otfs,
		propertiesBufferFeatures = FeatureFlagBits bfs }
