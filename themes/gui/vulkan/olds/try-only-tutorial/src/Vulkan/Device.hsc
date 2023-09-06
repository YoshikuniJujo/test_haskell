{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan (Queue)
import Vulkan.Base
import Vulkan.PhysicalDevice (PhysicalDevice)

import qualified Vulkan.Device.Queue as Device.Queue
import qualified Vulkan.PhysicalDevice as PhysicalDevice

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkDeviceCreateInfo}
		#{alignment VkDeviceCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDeviceCreateInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkDeviceCreateInfo, pNext} |],
		[| #{poke VkDeviceCreateInfo, pNext} |]),
	("flags", ''#{type VkDeviceCreateFlags},
		[| #{peek VkDeviceCreateInfo, flags} |],
		[| #{poke VkDeviceCreateInfo, flags} |]),
	("queueCreateInfoCount", ''#{type uint32_t},
		[| #{peek VkDeviceCreateInfo, queueCreateInfoCount} |],
		[| #{poke VkDeviceCreateInfo, queueCreateInfoCount} |]),
	("pQueueCreateInfos", ''Device.Queue.PtrCreateInfo,
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
	("pEnabledFeatures", ''PhysicalDevice.PtrFeatures,
		[| #{peek VkDeviceCreateInfo, pEnabledFeatures} |],
		[| #{poke VkDeviceCreateInfo, pEnabledFeatures} |])
	]
	[''Show]

data DeviceTag
type Device = Ptr DeviceTag

foreign import ccall "vkCreateDevice" create ::
	PhysicalDevice -> Ptr CreateInfo -> Ptr () -> Ptr Device ->
	IO #{type VkResult}

foreign import ccall "vkDestroyDevice" destroy :: Device -> Ptr () -> IO ()

foreign import ccall "vkGetDeviceQueue" getQueue ::
	Device -> #{type uint32_t} -> #{type uint32_t} -> Ptr Queue -> IO ()

foreign import ccall "vkDeviceWaitIdle" waitIdle ::
	Device -> IO #{type VkResult}
