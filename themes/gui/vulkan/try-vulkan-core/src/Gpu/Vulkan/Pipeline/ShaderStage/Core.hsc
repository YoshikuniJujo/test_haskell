{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ShaderStage.Core (

	-- * CREATE INFO

	CreateInfo, PtrCreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoStage, createInfoModule, createInfoPName,
	createInfoPSpecializationInfo

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

import qualified Gpu.Vulkan.ShaderModule.Core as Shader.Module
import qualified Gpu.Vulkan.Specialization.Core as Specialization

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO}

struct "CreateInfo" #{size VkPipelineShaderStageCreateInfo}
		#{alignment VkPipelineShaderStageCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineShaderStageCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineShaderStageCreateInfo, pNext} |],
		[| #{poke VkPipelineShaderStageCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineShaderStageCreateFlags},
		[| #{peek VkPipelineShaderStageCreateInfo, flags} |],
		[| #{poke VkPipelineShaderStageCreateInfo, flags} |]),
	("stage", ''#{type VkShaderStageFlagBits},
		[| #{peek VkPipelineShaderStageCreateInfo, stage} |],
		[| #{poke VkPipelineShaderStageCreateInfo, stage} |]),
	("module", ''Shader.Module.S,
		[| #{peek VkPipelineShaderStageCreateInfo, module} |],
		[| #{poke VkPipelineShaderStageCreateInfo, module} |]),
	("pName", ''CString,
		[| #{peek VkPipelineShaderStageCreateInfo, pName} |],
		[| #{poke VkPipelineShaderStageCreateInfo, pName} |]),
	("pSpecializationInfo", ''Specialization.PtrInfo,
		[| #{peek VkPipelineShaderStageCreateInfo,
			pSpecializationInfo} |],
		[| #{poke VkPipelineShaderStageCreateInfo,
			pSpecializationInfo} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
