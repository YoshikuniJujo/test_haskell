-- This file is automatically generated by the tools/makeEnumVkShaderStageFlagBits.hs
--	% stack runghc --cwd tools/ makeEnumVkShaderStageFlagBits.hs

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.ShaderStageFlagBits where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word

#include <vulkan/vulkan.h>

enum "ShaderStageFlagBits" ''#{type VkShaderStageFlagBits} [''Show, ''Eq, ''Storable] [
	("ShaderStageVertexBit", #{const VK_SHADER_STAGE_VERTEX_BIT}),
	("ShaderStageTessellationControlBit",
		#{const VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT}),
	("ShaderStageTessellationEvaluationBit",
		#{const VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT}),
	("ShaderStageGeometryBit", #{const VK_SHADER_STAGE_GEOMETRY_BIT}),
	("ShaderStageFragmentBit", #{const VK_SHADER_STAGE_FRAGMENT_BIT}),
	("ShaderStageComputeBit", #{const VK_SHADER_STAGE_COMPUTE_BIT}),
	("ShaderStageAllGraphics", #{const VK_SHADER_STAGE_ALL_GRAPHICS}),
	("ShaderStageAll", #{const VK_SHADER_STAGE_ALL}),
	("ShaderStageRaygenBitKhr", #{const VK_SHADER_STAGE_RAYGEN_BIT_KHR}),
	("ShaderStageAnyHitBitKhr", #{const VK_SHADER_STAGE_ANY_HIT_BIT_KHR}),
	("ShaderStageClosestHitBitKhr",
		#{const VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR}),
	("ShaderStageMissBitKhr", #{const VK_SHADER_STAGE_MISS_BIT_KHR}),
	("ShaderStageIntersectionBitKhr",
		#{const VK_SHADER_STAGE_INTERSECTION_BIT_KHR}),
	("ShaderStageCallableBitKhr",
		#{const VK_SHADER_STAGE_CALLABLE_BIT_KHR}),
	("ShaderStageTaskBitNv", #{const VK_SHADER_STAGE_TASK_BIT_NV}),
	("ShaderStageMeshBitNv", #{const VK_SHADER_STAGE_MESH_BIT_NV}),
	("ShaderStageSubpassShadingBitHuawei",
		#{const VK_SHADER_STAGE_SUBPASS_SHADING_BIT_HUAWEI}),
	("ShaderStageRaygenBitNv", #{const VK_SHADER_STAGE_RAYGEN_BIT_NV}),
	("ShaderStageAnyHitBitNv", #{const VK_SHADER_STAGE_ANY_HIT_BIT_NV}),
	("ShaderStageClosestHitBitNv",
		#{const VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV}),
	("ShaderStageMissBitNv", #{const VK_SHADER_STAGE_MISS_BIT_NV}),
	("ShaderStageIntersectionBitNv",
		#{const VK_SHADER_STAGE_INTERSECTION_BIT_NV}),
	("ShaderStageCallableBitNv", #{const VK_SHADER_STAGE_CALLABLE_BIT_NV}),
	("ShaderStageFlagBitsMaxEnum",
		#{const VK_SHADER_STAGE_FLAG_BITS_MAX_ENUM}) ]

type ShaderStageFlags = ShaderStageFlagBits