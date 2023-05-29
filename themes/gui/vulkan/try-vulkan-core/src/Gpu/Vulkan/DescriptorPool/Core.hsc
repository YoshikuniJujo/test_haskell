{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorPool.Core (

	-- * CREATE AND DESTROY

	create, destroy, D, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoMaxSets, createInfoPoolSizeCount, createInfoPPoolSizes,

	-- ** Size

	Size, PtrSize, pattern Size, sizeType, sizeDescriptorCount

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device

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

data DTag
type D = Ptr DTag

foreign import ccall "vkCreateDescriptorPool" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A ->
	Ptr D -> IO #{type VkResult}

foreign import ccall "vkDestroyDescriptorPool" destroy ::
	Device.D -> D -> Ptr AllocationCallbacks.A -> IO ()
