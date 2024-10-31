{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Core (

	-- * CREATE AND DESTROY

	create, destroy, D, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoQueueCreateInfoCount, createInfoPQueueCreateInfos,
	createInfoEnabledLayerCount, createInfoPpEnabledLayerNames,
	createInfoEnabledExtensionCount, createInfoPpEnabledExtensionNames,
	createInfoPEnabledFeatures,

	-- ** QueueCreateInfo

	QueueCreateInfo, PtrQueueCreateInfo, pattern QueueCreateInfo,
	queueCreateInfoSType, queueCreateInfoPNext, queueCreateInfoFlags,
	queueCreateInfoQueueFamilyIndex,
	queueCreateInfoQueueCount, queueCreateInfoPQueuePriorities,

	-- * GET AND WAIT IDLE

	getQueue, waitIdle

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.PhysicalDevice.Core as PhysicalDevice
import qualified Gpu.Vulkan.PhysicalDevice.Struct.Core as PhysicalDevice
import qualified Gpu.Vulkan.Queue.Core as Queue

#include <vulkan/vulkan.h>

queueStructureType :: #{type VkStructureType}
queueStructureType = #{const VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO}

struct "QueueCreateInfo" #{size VkDeviceQueueCreateInfo} 
		#{alignment VkDeviceQueueCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDeviceQueueCreateInfo, sType}
			p queueStructureType |]),
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

type PtrQueueCreateInfo = Ptr QueueCreateInfo

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
	("pQueueCreateInfos", ''PtrQueueCreateInfo,
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
	[''Show, ''Storable]

data DTag
type D = Ptr DTag

foreign import ccall "vkCreateDevice" create ::
	PhysicalDevice.P -> Ptr CreateInfo -> Ptr AllocationCallbacks.A ->
	Ptr D -> IO #{type VkResult}

foreign import ccall "vkDestroyDevice"
	destroy :: D -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "vkGetDeviceQueue" getQueue ::
	D -> #{type uint32_t} -> #{type uint32_t} -> Ptr Queue.Q -> IO ()

foreign import ccall "vkDeviceWaitIdle" waitIdle ::
	D -> IO #{type VkResult}
