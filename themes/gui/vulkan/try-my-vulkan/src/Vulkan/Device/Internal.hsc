{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device.Internal where

import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Bits
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

enum "DeviceQueueCreateFlagBits" ''#{type VkDeviceQueueCreateFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("DeviceQueueCreateProtectedBit",
		#{const VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT}) ]

type DeviceQueueCreateFlags = DeviceQueueCreateFlagBits

struct "DeviceQueueCreateInfo" #{size VkDeviceQueueCreateInfo}
		#{alignment VkDeviceQueueCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDeviceQueueCreateInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkDeviceQueueCreateInfo, pNext} |],
		[| #{poke VkDeviceQueueCreateInfo, pNext} |]),
	("flags", ''DeviceQueueCreateFlags,
		[| #{peek VkDeviceQueueCreateInfo, flags} |],
		[| #{poke VkDeviceQueueCreateInfo, flags} |]),
	("queueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkDeviceQueueCreateInfo, queueFamilyIndex} |],
		[| #{poke VkDeviceQueueCreateInfo, queueFamilyIndex} |]),
	("queueCount", ''#{type uint32_t},
		[| #{peek VkDeviceQueueCreateInfo, queueCount} |],
		[| #{poke VkDeviceQueueCreateInfo, queueCount} |]),
	("pQueuePriorities", ''PtrCFloat,
		[| #{peek VkDeviceQueueCreateInfo, pQueuePriorities} |],
		[| #{poke VkDeviceQueueCreateInfo, pQueuePriorities} |]) ]
	[''Show]
