{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device.Queue where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

structureType :: #{type VkStructureType}
structureType = #{const VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO}

struct "CreateInfo" #{size VkDeviceQueueCreateInfo} 
		#{alignment VkDeviceQueueCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDeviceQueueCreateInfo, sType}
			p structureType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDeviceQueueCreateInfo, pNext} |],
		[| #{poke VkDeviceQueueCreateInfo, pNext} |]),
	("flags", ''#{type VkDeviceQueueCreateFlags},
		[| #{peek VkDeviceQueueCreateInfo, flags} |],
		[| #{poke VkDeviceQueueCreateInfo, flags} |]),
	("queueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkDeviceQueueCreateInfo, queueFamilyIndex} |],
		[| #{poke VkDeviceQueueCreateInfo, queueFamilyIndex} |]),
	("queueCount", ''#{type uint32_t},
		[| #{peek VkDeviceQueueCreateInfo, queueCount} |],
		[| #{poke VkDeviceQueueCreateInfo, queueCount} |]),
	("pQueuePriorities", ''PtrFloat,
		[| #{peek VkDeviceQueueCreateInfo, pQueuePriorities} |],
		[| #{poke VkDeviceQueueCreateInfo, pQueuePriorities} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
