{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.RasterizationState.Core (

	-- * CREATE INFO

	CreateInfo, PtrCreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoDepthClampEnable, createInfoRasterizerDiscardEnable,
	createInfoPolygonMode, createInfoCullMode, createInfoFrontFace,
	createInfoDepthBiasEnable, createInfoDepthBiasConstantFactor,
	createInfoDepthBiasClamp, createInfoDepthBiasSlopeFactor,
	createInfoLineWidth

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO}

struct "CreateInfo" #{size VkPipelineRasterizationStateCreateInfo}
		#{alignment VkPipelineRasterizationStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineRasterizationStateCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineRasterizationStateCreateInfo, pNext} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineRasterizationStateCreateFlags},
		[| #{peek VkPipelineRasterizationStateCreateInfo, flags} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo, flags} |]),
	("depthClampEnable", ''#{type VkBool32},
		[| #{peek VkPipelineRasterizationStateCreateInfo,
			depthClampEnable} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			depthClampEnable} |]),
	("rasterizerDiscardEnable", ''#{type VkBool32},
		[| #{peek VkPipelineRasterizationStateCreateInfo,
			rasterizerDiscardEnable} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			rasterizerDiscardEnable} |]),
	("polygonMode", ''#{type VkPolygonMode},
		[| #{peek VkPipelineRasterizationStateCreateInfo,
			polygonMode} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			polygonMode} |]),
	("cullMode", ''#{type VkCullModeFlags},
		[| #{peek VkPipelineRasterizationStateCreateInfo, cullMode} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo, cullMode} |]),
	("frontFace", ''#{type VkFrontFace},
		[| #{peek VkPipelineRasterizationStateCreateInfo, frontFace} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			frontFace} |]),
	("depthBiasEnable", ''#{type VkBool32},
		[| #{peek VkPipelineRasterizationStateCreateInfo,
			depthBiasEnable} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			depthBiasEnable} |]),
	("depthBiasConstantFactor", ''#{type float},
		[| #{peek VkPipelineRasterizationStateCreateInfo,
			depthBiasConstantFactor} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			depthBiasConstantFactor} |]),
	("depthBiasClamp", ''#{type float},
		[| #{peek VkPipelineRasterizationStateCreateInfo,
			depthBiasClamp } |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			depthBiasClamp } |]),
	("depthBiasSlopeFactor", ''#{type float},
		[| #{peek VkPipelineRasterizationStateCreateInfo,
			depthBiasSlopeFactor} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			depthBiasSlopeFactor} |]),
	("lineWidth", ''#{type float},
		[| #{peek VkPipelineRasterizationStateCreateInfo, lineWidth} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo, lineWidth} |])
	]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
