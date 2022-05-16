{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Pool.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base

import qualified Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Vulkan.Device.Core as Device

#include <vulkan/vulkan.h>

struct "Size" #{size VkDescriptorPoolSize} #{alignment VkDescriptorPoolSize} [
	("type", ''#{type VkDescriptorType},
		[| #{peek VkDescriptorPoolSize, type} |],
		[| #{poke VkDescriptorPoolSize, type} |]),
	("descriptorCount", ''#{type uint32_t},
		[| #{peek VkDescriptorPoolSize, descriptorCount} |],
		[| #{poke VkDescriptorPoolSize, descriptorCount} |])
	]
	[''Show, ''Storable]

type PtrSize = Ptr Size

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO}

struct "CreateInfo" #{size VkDescriptorPoolCreateInfo}
		#{alignment VkDescriptorPoolCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ ->
			#{poke VkDescriptorPoolCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDescriptorPoolCreateInfo, pNext} |],
		[| #{poke VkDescriptorPoolCreateInfo, pNext} |]),
	("flags", ''#{type VkDescriptorPoolCreateFlags},
		[| #{peek VkDescriptorPoolCreateInfo, flags} |],
		[| #{poke VkDescriptorPoolCreateInfo, flags} |]),
	("maxSets", ''#{type uint32_t},
		[| #{peek VkDescriptorPoolCreateInfo, maxSets} |],
		[| #{poke VkDescriptorPoolCreateInfo, maxSets} |]),
	("poolSizeCount", ''#{type uint32_t},
		[| #{peek VkDescriptorPoolCreateInfo, poolSizeCount} |],
		[| #{poke VkDescriptorPoolCreateInfo, poolSizeCount} |]),
	("pPoolSizes", ''PtrSize,
		[| #{peek VkDescriptorPoolCreateInfo, pPoolSizes} |],
		[| #{poke VkDescriptorPoolCreateInfo, pPoolSizes} |]) ]
	[''Show, ''Storable]

data PTag
type P = Ptr PTag

foreign import ccall "vkCreateDescriptorPool" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A ->
	Ptr P -> IO #{type VkResult}

foreign import ccall "vkDestroyDescriptorPool" destroy ::
	Device.D -> P -> Ptr AllocationCallbacks.A -> IO ()
