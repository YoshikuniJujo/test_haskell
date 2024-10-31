{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.Core (

	-- * CREATE INFO

	CreateInfo, PtrCreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoVertexBindingDescriptionCount,
	createInfoPVertexBindingDescriptions,
	createInfoVertexAttributeDescriptionCount,
	createInfoPVertexAttributeDescriptions

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct	
import Foreign.C.Struct.TypeSynonyms
import Data.Word

import qualified Gpu.Vulkan.VertexInput.Core as VertexInput

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO}

struct "CreateInfo" #{size VkPipelineVertexInputStateCreateInfo}
		#{alignment VkPipelineVertexInputStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineVertexInputStateCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineVertexInputStateCreateInfo, pNext} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineVertexInputStateCreateFlags},
		[| #{peek VkPipelineVertexInputStateCreateInfo, flags} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo, flags} |]),
	("vertexBindingDescriptionCount", ''#{type uint32_t},
		[| #{peek VkPipelineVertexInputStateCreateInfo,
			vertexBindingDescriptionCount} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo,
			vertexBindingDescriptionCount} |]),
	("pVertexBindingDescriptions", ''VertexInput.PtrBindingDescription,
		[| #{peek VkPipelineVertexInputStateCreateInfo,
			pVertexBindingDescriptions} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo,
			pVertexBindingDescriptions} |]),
	("vertexAttributeDescriptionCount", ''#{type uint32_t},
		[| #{peek VkPipelineVertexInputStateCreateInfo,
			vertexAttributeDescriptionCount} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo,
			vertexAttributeDescriptionCount} |]),
	("pVertexAttributeDescriptions", ''VertexInput.PtrAttributeDescription,
		[| #{peek VkPipelineVertexInputStateCreateInfo,
			pVertexAttributeDescriptions} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo,
			pVertexAttributeDescriptions} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
