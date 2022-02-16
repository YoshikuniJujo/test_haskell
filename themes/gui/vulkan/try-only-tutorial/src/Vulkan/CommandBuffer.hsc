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

import Vulkan.Base
import Vulkan.Device (Device)

import qualified Vulkan.CommandPool as CommandPool

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO}

struct "AllocateInfo" #{size VkCommandBufferAllocateInfo}
		#{alignment VkCommandBufferAllocateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandBufferAllocateInfo, sType}
			p sType |]),
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

data CommandBufferTag
type CommandBuffer = Ptr CommandBufferTag

levelPrimary :: #{type VkCommandBufferLevel}
levelPrimary = #{const VK_COMMAND_BUFFER_LEVEL_PRIMARY}

foreign import ccall "vkAllocateCommandBuffers" allocate ::
	Device -> Ptr AllocateInfo -> Ptr CommandBuffer -> IO #{type VkResult}
