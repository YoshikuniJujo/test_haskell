{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Compute.Core (

	-- * CREATE

	createCs, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoStage, createInfoLayout,
	createInfoBasePipelineHandle, createInfoBasePipelineIndex

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.Pipeline.Core as Pipeline
import qualified Gpu.Vulkan.Pipeline.ShaderStage.Core as Pipeline.ShaderStage
import qualified Gpu.Vulkan.PipelineLayout.Core as Pipeline.Layout
import qualified Gpu.Vulkan.PipelineCache.Core as Cache

#include <vulkan/vulkan.h>

stype :: #{type VkStructureType}
stype = #{const VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO}

struct "CreateInfo" #{size VkComputePipelineCreateInfo}
		#{alignment VkComputePipelineCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkComputePipelineCreateInfo, sType}
			p stype |]),
	("pNext", ''PtrVoid,
		[| #{peek VkComputePipelineCreateInfo, pNext} |],
		[| #{poke VkComputePipelineCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineCreateFlags},
		[| #{peek VkComputePipelineCreateInfo, flags} |],
		[| #{poke VkComputePipelineCreateInfo, flags} |]),
	("stage", ''Pipeline.ShaderStage.CreateInfo,
		[| #{peek VkComputePipelineCreateInfo, stage} |],
		[| #{poke VkComputePipelineCreateInfo, stage} |]),
	("layout", ''Pipeline.Layout.P,
		[| #{peek VkComputePipelineCreateInfo, layout} |],
		[| #{poke VkComputePipelineCreateInfo, layout} |]),
	("basePipelineHandle", ''Pipeline.P,
		[| #{peek VkComputePipelineCreateInfo, basePipelineHandle} |],
		[| #{poke VkComputePipelineCreateInfo, basePipelineHandle} |]),
	("basePipelineIndex", ''#{type int32_t},
		[| #{peek VkComputePipelineCreateInfo, basePipelineIndex} |],
		[| #{poke VkComputePipelineCreateInfo, basePipelineIndex} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateComputePipelines" createCs ::
	Device.D -> Cache.P -> #{type uint32_t} -> Ptr CreateInfo ->
	Ptr AllocationCallbacks.A -> Ptr Pipeline.P -> IO #{type VkResult}
