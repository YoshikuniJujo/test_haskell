{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Buffer.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base

import qualified Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Vulkan.Device.Core as Device
import qualified Vulkan.Memory.Core as Memory

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkBufferCreateInfo} #{alignment VkBufferCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkBufferCreateInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkBufferCreateInfo, pNext} |],
		[| #{poke VkBufferCreateInfo, pNext} |]),
	("flags", ''#{type VkBufferCreateFlags},
		[| #{peek VkBufferCreateInfo, flags} |],
		[| #{poke VkBufferCreateInfo, flags} |]),
	("size", ''#{type VkDeviceSize},
		[| #{peek VkBufferCreateInfo, size} |],
		[| #{poke VkBufferCreateInfo, size} |]),
	("usage", ''#{type VkBufferUsageFlags},
		[| #{peek VkBufferCreateInfo, usage} |],
		[| #{poke VkBufferCreateInfo, usage} |]),
	("sharingMode", ''#{type VkSharingMode},
		[| #{peek VkBufferCreateInfo, sharingMode} |],
		[| #{poke VkBufferCreateInfo, sharingMode} |]),
	("queueFamilyIndexCount", ''#{type uint32_t},
		[| #{peek VkBufferCreateInfo, queueFamilyIndexCount} |],
		[| #{poke VkBufferCreateInfo, queueFamilyIndexCount} |]),
	("pQueueFamilyIndices", ''PtrUint32T,
		[| #{peek VkBufferCreateInfo, pQueueFamilyIndices} |],
		[| #{poke VkBufferCreateInfo, pQueueFamilyIndices} |]) ]
	[''Show, ''Storable]

data BTag
type B = Ptr BTag

foreign import ccall "vkCreateBuffer" create ::
	Device.D -> Ptr CreateInfo ->  Ptr AllocationCallbacks.A -> Ptr B ->
	IO #{type VkResult}

foreign import ccall "vkDestroyBuffer" destroy ::
	Device.D -> B -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "vkGetBufferMemoryRequirements" getMemoryRequirements ::
	Device.D -> B -> Ptr Memory.Requirements -> IO ()

foreign import ccall "vkBindBufferMemory" bindMemory ::
	Device.D -> B -> Device.Memory -> #{type VkDeviceSize} ->
	IO #{type VkResult}
