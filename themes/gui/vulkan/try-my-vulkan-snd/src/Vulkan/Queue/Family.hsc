{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Queue.Family where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

import Vulkan

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
