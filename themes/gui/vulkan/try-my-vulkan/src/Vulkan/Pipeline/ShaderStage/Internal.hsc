{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ShaderStage.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.Shader
import Vulkan.ShaderStageFlagBits
import Vulkan.SpecializationInfo.Internal
import Vulkan.Pipeline.ShaderStage.Variables

#include <vulkan/vulkan.h>

enum "PipelineShaderStageCreateFlagBits"
		''#{type VkPipelineShaderStageCreateFlagBits}
		[''Show, ''Storable] [
	("PipelineShaderStageCreateFlagBitsZero", 0),
	("PipelineShaderStageCreateAllowVaryingSubgroupSizeBitExt",
		pipelineShaderStageCreateAllowVaryingSubgroupSizeBitExt),
	("PipelineshaderStageCreateRequireFullSubgroupsBitExt", #{const
		VK_PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT})
	]

type PipelineShaderStageCreateFlags = PipelineShaderStageCreateFlagBits
type PtrSpecializationInfo = Ptr SpecializationInfo

struct "CreateInfo" #{size VkPipelineShaderStageCreateInfo}
		#{alignment VkPipelineShaderStageCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineShaderStageCreateInfo, sType} p
			(#{const
			VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO} ::
			#{type VkStructureType}) |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineShaderStageCreateInfo, pNext} |],
		[| #{poke VkPipelineShaderStageCreateInfo, pNext} |]),
	("flags", ''PipelineShaderStageCreateFlags,
		[| #{peek VkPipelineShaderStageCreateInfo, flags} |],
		[| #{poke VkPipelineShaderStageCreateInfo, flags} |]),
	("stage", ''ShaderStageFlags,
		[| #{peek VkPipelineShaderStageCreateInfo, stage} |],
		[| #{poke VkPipelineShaderStageCreateInfo, stage} |]),
	("module", ''ShaderModule,
		[| #{peek VkPipelineShaderStageCreateInfo, module} |],
		[| #{poke VkPipelineShaderStageCreateInfo, module} |]),
	("pName", ''CString,
		[| #{peek VkPipelineShaderStageCreateInfo, pName} |],
		[| #{poke VkPipelineShaderStageCreateInfo, pName} |]),
	("pSpecializationInfo", ''PtrSpecializationInfo,
		[| #{peek VkPipelineShaderStageCreateInfo,
			pSpecializationInfo} |],
		[| #{poke VkPipelineShaderStageCreateInfo,
			pSpecializationInfo} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
