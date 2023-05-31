{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.QueueFamily.Core (

	-- * PROPERTIES

	Properties, pattern Properties,
	propertiesQueueFlags, propertiesQueueCount,
	propertiesTimestampValidBits, propertiesMinImageTransferGranularity

	) where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

import Gpu.Vulkan.Core

struct "Properties" #{size VkQueueFamilyProperties}
		#{alignment VkQueueFamilyProperties} [
	("queueFlags", ''#{type VkQueueFlags},
		[| #{peek VkQueueFamilyProperties, queueFlags} |],
		[| #{poke VkQueueFamilyProperties, queueFlags} |]),
	("queueCount", ''#{type uint32_t},
		[| #{peek VkQueueFamilyProperties, queueCount} |],
		[| #{poke VkQueueFamilyProperties, queueCount} |]),
	("timestampValidBits", ''#{type uint32_t},
		[| #{peek VkQueueFamilyProperties, timestampValidBits} |],
		[| #{poke VkQueueFamilyProperties, timestampValidBits} |]),
	("minImageTransferGranularity", ''Extent3d,
		[| #{peek VkQueueFamilyProperties,
			minImageTransferGranularity} |],
		[| #{poke VkQueueFamilyProperties,
			minImageTransferGranularity} |]) ]
	[''Show, ''Storable]
