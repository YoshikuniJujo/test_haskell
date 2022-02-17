{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan (CommandBuffer)
import Vulkan.Base
import Vulkan.Device (Device)

import qualified Vulkan.CommandPool as CommandPool

#include <vulkan/vulkan.h>

sTypeA, sTypeB :: #{type VkStructureType}
sTypeA = #{const VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO}
sTypeB = #{const VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO}

struct "AllocateInfo" #{size VkCommandBufferAllocateInfo}
		#{alignment VkCommandBufferAllocateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandBufferAllocateInfo, sType}
			p sTypeA |]),
	("pNext", ''PtrVoid,
		[| #{peek VkCommandBufferAllocateInfo, pNext} |],
		[| #{poke VkCommandBufferAllocateInfo, pNext} |]),
	("commandPool", ''CommandPool.CommandPool,
		[| #{peek VkCommandBufferAllocateInfo, commandPool} |],
		[| #{poke VkCommandBufferAllocateInfo, commandPool} |]),
	("level", ''#{type VkCommandBufferLevel},
		[| #{peek VkCommandBufferAllocateInfo, level} |],
		[| #{poke VkCommandBufferAllocateInfo, level} |]),
	("commandBufferCount", ''#{type uint32_t},
		[| #{peek VkCommandBufferAllocateInfo, commandBufferCount} |],
		[| #{poke VkCommandBufferAllocateInfo, commandBufferCount} |]) ]
	[''Show, ''Storable]

levelPrimary :: #{type VkCommandBufferLevel}
levelPrimary = #{const VK_COMMAND_BUFFER_LEVEL_PRIMARY}

foreign import ccall "vkAllocateCommandBuffers" allocate ::
	Device -> Ptr AllocateInfo -> Ptr CommandBuffer -> IO #{type VkResult}

struct "InheritanceInfo" #{size VkCommandBufferInheritanceInfo}
		#{alignment VkCommandBufferInheritanceInfo} [
	]
	[''Show, ''Storable]

type PtrInheritanceInfo = Ptr InheritanceInfo

struct "BeginInfo" #{size VkCommandBufferBeginInfo}
		#{alignment VkCommandBufferBeginInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandBufferBeginInfo, sType}
			p sTypeB |]),
	("pNext", ''PtrVoid,
		[| #{peek VkCommandBufferBeginInfo, pNext} |],
		[| #{poke VkCommandBufferBeginInfo, pNext} |]),
	("flags", ''#{type VkCommandBufferUsageFlags},
		[| #{peek VkCommandBufferBeginInfo, flags} |],
		[| #{poke VkCommandBufferBeginInfo, flags} |]),
	("pInheritanceInfo", ''PtrInheritanceInfo,
		[| #{peek VkCommandBufferBeginInfo, pInheritanceInfo} |],
		[| #{poke VkCommandBufferBeginInfo, pInheritanceInfo} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkBeginCommandBuffer" begin ::
	CommandBuffer -> Ptr BeginInfo -> IO #{type VkResult}

foreign import ccall "vkEndCommandBuffer" end ::
	CommandBuffer -> IO #{type VkResult}
