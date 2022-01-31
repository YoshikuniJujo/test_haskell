{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_gHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.RasterizationState.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.PolygonMode
import Vulkan.CullModeFlagBits
import Vulkan.FrontFace

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineRasterizationStateCreateFlags}
	[''Show, ''Storable] [("CreateFlagsZero", 0)]

type PtrCreateFlags = CreateFlags

struct "CreateInfo" #{size VkPipelineRasterizationStateCreateInfo}
		#{alignment VkPipelineRasterizationStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineRasterizationStateCreateInfo, sType}
			p ST.pipelineRasterizationStateCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineRasterizationStateCreateInfo, pNext} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo, pNext} |]),
	("flags", ''PtrCreateFlags,
		[| #{peek VkPipelineRasterizationStateCreateInfo, flags} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo, flags} |]),
	("depthClampEnable", ''Bool32,
		[| #{peek VkPipelineRasterizationStateCreateInfo,
			depthClampEnable} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			depthClampEnable} |]),
	("rasterizerDiscardEnable", ''Bool32,
		[| #{peek VkPipelineRasterizationStateCreateInfo,
			rasterizerDiscardEnable} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			rasterizerDiscardEnable} |]),
	("polygonMode", ''PolygonMode,
		[| #{peek VkPipelineRasterizationStateCreateInfo,
			polygonMode} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			polygonMode} |]),
	("cullMode", ''CullModeFlags,
		[| #{peek VkPipelineRasterizationStateCreateInfo, cullMode} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo, cullMode} |]),
	("frontFace", ''FrontFace,
		[| #{peek VkPipelineRasterizationStateCreateInfo, frontFace} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			frontFace} |]),
	("depthBiasEnable", ''Bool32,
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
			depthBiasClamp} |],
		[| #{poke VkPipelineRasterizationStateCreateInfo,
			depthBiasClamp} |]),
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
