{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Bits
import Data.Word

import Vulkan.Base

import Vulkan.PhysicalDevice

#include <vulkan/vulkan.h>

enum "DeviceQueueCreateFlagBits" ''#{type VkDeviceQueueCreateFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("DeviceQueueCreateFlagBitsZero", 0),
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
	[''Show, ''Storable]

enum "DeviceCreateFlagBits" ''#{type VkDeviceCreateFlags} [''Show, ''Storable] [
	("DeviceCreateFlagBitsZero", 0) ]

type DeviceCreateFlags = DeviceCreateFlagBits

type PtrDeviceQueueCreateInfo = Ptr DeviceQueueCreateInfo
type PtrPhysicalDeviceFeatures = Ptr PhysicalDeviceFeatures

struct "DeviceCreateInfo" #{size VkDeviceCreateInfo}
		#{alignment VkDeviceCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDeviceCreateInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkDeviceCreateInfo, pNext} |],
		[| #{poke VkDeviceCreateInfo, pNext} |]),
	("flags", ''DeviceCreateFlags, [| #{peek VkDeviceCreateInfo, flags} |],
		[| #{poke VkDeviceCreateInfo, flags} |]),
	("queueCreateInfoCount", ''#{type uint32_t},
		[| #{peek VkDeviceCreateInfo, queueCreateInfoCount} |],
		[| #{poke VkDeviceCreateInfo, queueCreateInfoCount} |]),
	("pQueueCreateInfos", ''PtrDeviceQueueCreateInfo,
		[| #{peek VkDeviceCreateInfo, pQueueCreateInfos} |],
		[| #{poke VkDeviceCreateInfo, pQueueCreateInfos} |]),
	("enabledLayerCount", ''#{type uint32_t},
		[| #{peek VkDeviceCreateInfo, enabledLayerCount} |],
		[| #{poke VkDeviceCreateInfo, enabledLayerCount} |]),
	("ppEnabledLayerNames", ''PtrCString,
		[| #{peek VkDeviceCreateInfo, ppEnabledLayerNames} |],
		[| #{poke VkDeviceCreateInfo, ppEnabledLayerNames} |]),
	("enabledExtensionCount", ''#{type uint32_t},
		[| #{peek VkDeviceCreateInfo, enabledExtensionCount} |],
		[| #{poke VkDeviceCreateInfo, enabledExtensionCount} |]),
	("ppEnabledExtensionNames", ''PtrCString,
		[| #{peek VkDeviceCreateInfo, ppEnabledExtensionNames} |],
		[| #{poke VkDeviceCreateInfo, ppEnabledExtensionNames} |]),
	("pEnabledFeatures", ''PtrPhysicalDeviceFeatures,
		[| #{peek VkDeviceCreateInfo, pEnabledFeatures} |],
		[| #{poke VkDeviceCreateInfo, pEnabledFeatures} |])
	]
	[''Show]
