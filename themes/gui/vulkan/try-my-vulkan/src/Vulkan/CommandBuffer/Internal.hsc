{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.CommandPool
import Vulkan.CommandBufferLevel

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "AllocateInfo" #{size VkCommandBufferAllocateInfo}
		#{alignment VkCommandBufferAllocateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandBufferAllocateInfo, sType}
			p ST.commandBufferAllocateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkCommandBufferAllocateInfo, pNext} |],
		[| #{poke VkCommandBufferAllocateInfo, pNext} |]),
	("commandPool", ''CommandPool,
		[| #{peek VkCommandBufferAllocateInfo, commandPool} |],
		[| #{poke VkCommandBufferAllocateInfo, commandPool} |]),
	("level", ''CommandBufferLevel,
		[| #{peek VkCommandBufferAllocateInfo, level} |],
		[| #{poke VkCommandBufferAllocateInfo, level} |]),
	("commandBufferCount", ''#{type uint32_t},
		[| #{peek VkCommandBufferAllocateInfo, commandBufferCount} |],
		[| #{poke VkCommandBufferAllocateInfo, commandBufferCount} |]) ]
	[''Show, ''Storable]
