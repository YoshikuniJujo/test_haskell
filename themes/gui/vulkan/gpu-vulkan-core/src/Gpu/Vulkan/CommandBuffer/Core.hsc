{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Core (

	-- * ALLOCATE AND FREE

	allocateCs, freeCs, C, PtrC,
	AllocateInfo, pattern AllocateInfo,
	allocateInfoSType, allocateInfoPNext, allocateInfoCommandPool,
	allocateInfoLevel, levelPrimary, allocateInfoCommandBufferCount,

	-- * BEGIN, END AND RESET
	begin, end, reset,
	BeginInfo, pattern BeginInfo,
	beginInfoSType, beginInfoPNext, beginInfoFlags,
	beginInfoPInheritanceInfo,

	-- ** INHERITANCE INFO
	InheritanceInfo, pattern InheritanceInfo,
	inheritanceInfoSType, inheritanceInfoPNext,
	inheritanceInfoRenderPass, inheritanceInfoSubpass,
	inheritanceInfoFramebuffer, inheritanceInfoOcclusionQueryEnable,
	inheritanceInfoQueryFlags, inheritanceInfoPipelineStatistics

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.RenderPass.Core as RenderPass
import qualified Gpu.Vulkan.Framebuffer.Core as Framebuffer
import qualified Gpu.Vulkan.CommandPool.Core as CommandPool

#include <vulkan/vulkan.h>

sTypeA, sTypeB, sTypeI :: #{type VkStructureType}
sTypeA = #{const VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO}
sTypeB = #{const VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO}
sTypeI = #{const VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO}

struct "AllocateInfo" #{size VkCommandBufferAllocateInfo}
		#{alignment VkCommandBufferAllocateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandBufferAllocateInfo, sType}
			p sTypeA |]),
	("pNext", ''PtrVoid,
		[| #{peek VkCommandBufferAllocateInfo, pNext} |],
		[| #{poke VkCommandBufferAllocateInfo, pNext} |]),
	("commandPool", ''CommandPool.C,
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

data CTag
type C = Ptr CTag
type PtrC = Ptr C

foreign import ccall "vkAllocateCommandBuffers" allocateCs ::
	Device.D -> Ptr AllocateInfo -> Ptr C -> IO #{type VkResult}

struct "InheritanceInfo" #{size VkCommandBufferInheritanceInfo}
		#{alignment VkCommandBufferInheritanceInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandBufferInheritanceInfo, sType}
			p sTypeI |]),
	("pNext", ''PtrVoid,
		[| #{peek VkCommandBufferInheritanceInfo, pNext} |],
		[| #{poke VkCommandBufferInheritanceInfo, pNext} |]),
	("renderPass", ''RenderPass.R,
		[| #{peek VkCommandBufferInheritanceInfo, renderPass} |],
		[| #{poke VkCommandBufferInheritanceInfo, renderPass} |]),
	("subpass", ''#{type uint32_t},
		[| #{peek VkCommandBufferInheritanceInfo, subpass} |],
		[| #{poke VkCommandBufferInheritanceInfo, subpass} |]),
	("framebuffer", ''Framebuffer.F,
		[| #{peek VkCommandBufferInheritanceInfo, framebuffer} |],
		[| #{poke VkCommandBufferInheritanceInfo, framebuffer} |]),
	("occlusionQueryEnable", ''#{type VkBool32},
		[| #{peek VkCommandBufferInheritanceInfo,
			occlusionQueryEnable} |],
		[| #{poke VkCommandBufferInheritanceInfo,
			occlusionQueryEnable} |]),
	("queryFlags", ''#{type VkQueryControlFlags},
		[| #{peek VkCommandBufferInheritanceInfo, queryFlags} |],
		[| #{poke VkCommandBufferInheritanceInfo, queryFlags} |]),
	("pipelineStatistics", ''#{type VkQueryPipelineStatisticFlags},
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
	C -> Ptr BeginInfo -> IO #{type VkResult}

foreign import ccall "vkEndCommandBuffer" end :: C -> IO #{type VkResult}

foreign import ccall "vkResetCommandBuffer" reset ::
	C -> #{type VkCommandBufferResetFlags} -> IO #{type VkResult}

foreign import ccall "vkFreeCommandBuffers" freeCs ::
	Device.D -> CommandPool.C -> #{type uint32_t} -> Ptr C -> IO ()
