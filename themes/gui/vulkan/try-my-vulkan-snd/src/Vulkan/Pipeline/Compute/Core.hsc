{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Compute.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base

import qualified Vulkan.Pipeline.Core as Pipeline
import qualified Vulkan.Pipeline.ShaderStage.Core as Pipeline.ShaderStage
import qualified Vulkan.Pipeline.Layout.Core as Pipeline.Layout

#include <vulkan/vulkan.h>

data CTag
type C = Ptr CTag

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
	("layout", ''Pipeline.Layout.L,
		[| #{peek VkComputePipelineCreateInfo, layout} |],
		[| #{poke VkComputePipelineCreateInfo, layout} |]),
	("basePipelineHandle", ''Pipeline.P,
		[| #{peek VkComputePipelineCreateInfo, basePipelineHandle} |],
		[| #{poke VkComputePipelineCreateInfo, basePipelineHandle} |]),
	("basePipelineIndex", ''#{type int32_t},
		[| #{peek VkComputePipelineCreateInfo, basePipelineIndex} |],
		[| #{poke VkComputePipelineCreateInfo, basePipelineIndex} |]) ]
	[''Show, ''Storable]
