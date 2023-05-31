{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.QueryPool.Core (

	-- * CREATE AND DESTROY

	create, destroy, Q, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoQueryType, createInfoQueryCount, createInfoPipelineStatistics,

	-- * RESET AND GET RESULTS

	reset, getResults

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import Gpu.Vulkan.AllocationCallbacks.Core qualified as AllocationCallbacks
import Gpu.Vulkan.Device.Core qualified as Device

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO}

struct "CreateInfo" #{size VkQueryPoolCreateInfo}
	#{alignment VkQueryPoolCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkQueryPoolCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkQueryPoolCreateInfo, pNext} |],
		[| #{poke VkQueryPoolCreateInfo, pNext} |]),
	("flags", ''#{type VkQueryPoolCreateFlags},
		[| #{peek VkQueryPoolCreateInfo, flags} |],
		[| #{poke VkQueryPoolCreateInfo, flags} |]),
	("queryType", ''#{type VkQueryType},
		[| #{peek VkQueryPoolCreateInfo, queryType} |],
		[| #{poke VkQueryPoolCreateInfo, queryType} |]),
	("queryCount", ''#{type uint32_t},
		[| #{peek VkQueryPoolCreateInfo, queryCount} |],
		[| #{poke VkQueryPoolCreateInfo, queryCount} |]),
	("pipelineStatistics", ''#{type VkQueryPipelineStatisticFlags},
		[| #{peek VkQueryPoolCreateInfo, pipelineStatistics} |],
		[| #{poke VkQueryPoolCreateInfo, pipelineStatistics} |]) ]
	[''Show, ''Storable]

data QTag
type Q = Ptr QTag

foreign import ccall "vkCreateQueryPool" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr Q ->
	IO #{type VkResult}

foreign import ccall "vkDestroyQueryPool" destroy ::
	Device.D -> Q -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "vkResetQueryPool" reset ::
	Device.D -> Q -> #{type uint32_t} -> #{type uint32_t} -> IO ()

foreign import ccall "vkGetQueryPoolResults" getResults ::
	Device.D -> Q -> #{type uint32_t} -> #{type uint32_t} ->
	#{type size_t} -> Ptr () -> #{type VkDeviceSize} ->
	#{type VkQueryResultFlags} -> IO #{type VkResult}
