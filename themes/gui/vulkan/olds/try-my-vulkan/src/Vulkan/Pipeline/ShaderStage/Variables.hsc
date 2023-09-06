{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ShaderStage.Variables where

#include <vulkan/vulkan.h>

pipelineShaderStageCreateAllowVaryingSubgroupSizeBitExt :: Integral n => n
pipelineShaderStageCreateAllowVaryingSubgroupSizeBitExt = #{const
	VK_PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT}
