{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.CommandPool
import Vulkan.CommandBufferLevel
import Vulkan.Framebuffer (Framebuffer)
import Vulkan.QueryControlFlagBits
import Vulkan.QueryPipelineStatisticFlagBits
import Vulkan.CommandBufferUsageFlagBits

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

struct "InheritanceInfo" #{size VkCommandBufferInheritanceInfo}
		#{alignment VkCommandBufferInheritanceInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandBufferInheritanceInfo, sType}
			p ST.commandBufferInheritanceInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkCommandBufferInheritanceInfo, pNext} |],
		[| #{poke VkCommandBufferInheritanceInfo, pNext} |]),
	("renderPass", ''RenderPass,
		[| #{peek VkCommandBufferInheritanceInfo, renderPass} |],
		[| #{poke VkCommandBufferInheritanceInfo, renderPass} |]),
	("subpass", ''#{type uint32_t},
		[| #{peek VkCommandBufferInheritanceInfo, subpass} |],
		[| #{poke VkCommandBufferInheritanceInfo, subpass} |]),
	("framebuffer", ''Framebuffer,
		[| #{peek VkCommandBufferInheritanceInfo, framebuffer} |],
		[| #{poke VkCommandBufferInheritanceInfo, framebuffer} |]),
	("occlusionQueryEnable", ''Bool32,
		[| #{peek VkCommandBufferInheritanceInfo,
			occlusionQueryEnable} |],
		[| #{poke VkCommandBufferInheritanceInfo,
			occlusionQueryEnable} |]),
	("queryFlags", ''QueryControlFlags,
		[| #{peek VkCommandBufferInheritanceInfo, queryFlags} |],
		[| #{poke VkCommandBufferInheritanceInfo, queryFlags} |]),
	("pipelineStatistics", ''QueryPipelineStatisticFlags,
		[| #{peek VkCommandBufferInheritanceInfo,
			pipelineStatistics} |],
		[| #{poke VkCommandBufferInheritanceInfo,
			pipelineStatistics} |]) ]
	[''Show, ''Storable]

type PtrInheritanceInfo = Ptr InheritanceInfo

struct "BeginInfo" #{size VkCommandBufferBeginInfo}
		#{alignment VkCommandBufferBeginInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandBufferBeginInfo, sType}
			p ST.commandBufferBeginInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkCommandBufferBeginInfo, pNext} |],
		[| #{poke VkCommandBufferBeginInfo, pNext} |]),
	("flags", ''CommandBufferUsageFlags,
		[| #{peek VkCommandBufferBeginInfo, flags} |],
		[| #{poke VkCommandBufferBeginInfo, flags} |]),
	("pInheritanceInfo", ''PtrInheritanceInfo,
		[| #{peek VkCommandBufferBeginInfo, pInheritanceInfo} |],
		[| #{poke VkCommandBufferBeginInfo, pInheritanceInfo} |]) ]
	[''Show, ''Storable]
