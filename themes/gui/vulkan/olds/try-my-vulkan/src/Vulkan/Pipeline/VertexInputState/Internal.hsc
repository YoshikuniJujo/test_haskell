{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.Format

import qualified Vulkan.StructureType as SType

#include <vulkan/vulkan.h>

enum "PipelineVertexInputStateCreateFlags"
		''#{type VkPipelineVertexInputStateCreateFlags}
		[''Show, ''Storable] [
	("PipelineVertexInputStateCreateFlagsZero", 0) ]

enum "VertexInputRate" ''#{type VkVertexInputRate} [''Show, ''Storable] [
	("VertexInputRateVertex", #{const VK_VERTEX_INPUT_RATE_VERTEX}),
	("VertexInputRateInstance", #{const VK_VERTEX_INPUT_RATE_INSTANCE}) ]

struct "VertexInputBindingDescription" #{size VkVertexInputBindingDescription}
		#{alignment VkVertexInputBindingDescription} [
	("binding", ''#{type uint32_t},
		[| #{peek VkVertexInputBindingDescription, binding} |],
		[| #{poke VkVertexInputBindingDescription, binding} |]),
	("stride", ''#{type uint32_t},
		[| #{peek VkVertexInputBindingDescription, stride} |],
		[| #{poke VkVertexInputBindingDescription, stride} |]),
	("inputRate", ''VertexInputRate,
		[| #{peek VkVertexInputBindingDescription, inputRate} |],
		[| #{poke VkVertexInputBindingDescription, inputRate} |]) ]
	[''Show, ''Storable]

type PtrVertexInputBindingDescription = Ptr VertexInputBindingDescription

struct "VertexInputAttributeDescription"
		#{size VkVertexInputAttributeDescription}
		#{alignment VkVertexInputAttributeDescription} [
	("location", ''#{type uint32_t},
		[| #{peek VkVertexInputAttributeDescription, location} |],
		[| #{poke VkVertexInputAttributeDescription, location} |]),
	("binding", ''#{type uint32_t},
		[| #{peek VkVertexInputAttributeDescription, binding} |],
		[| #{poke VkVertexInputAttributeDescription, binding} |]),
	("format", ''Format,
		[| #{peek VkVertexInputAttributeDescription, format} |],
		[| #{poke VkVertexInputAttributeDescription, format} |]),
	("offset", ''#{type uint32_t},
		[| #{peek VkVertexInputAttributeDescription, offset} |],
		[| #{poke VkVertexInputAttributeDescription, offset} |]) ]
	[''Show, ''Storable]

type PtrVertexInputAttributeDescription = Ptr VertexInputAttributeDescription

struct "CreateInfo"
		#{size VkPipelineVertexInputStateCreateInfo}
		#{alignment VkPipelineVertexInputStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineVertexInputStateCreateInfo, sType}
			p SType.pipelineVertexInputStateCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineVertexInputStateCreateInfo, pNext} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo, pNext} |]),
	("flags", ''PipelineVertexInputStateCreateFlags,
		[| #{peek VkPipelineVertexInputStateCreateInfo, flags} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo, flags} |]),
	("vertexBindingDescriptionCount", ''#{type uint32_t},
		[| #{peek VkPipelineVertexInputStateCreateInfo,
			vertexBindingDescriptionCount} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo,
			vertexBindingDescriptionCount} |]),
	("pVertexBindingDescriptions", ''PtrVertexInputBindingDescription,
		[| #{peek VkPipelineVertexInputStateCreateInfo,
			pVertexBindingDescriptions} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo,
			pVertexBindingDescriptions} |]),
	("vertexAttributeDescriptionCount", ''#{type uint32_t},
		[| #{peek VkPipelineVertexInputStateCreateInfo,
			vertexAttributeDescriptionCount} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo,
			vertexAttributeDescriptionCount} |]),
	("pVertexAttributeDescriptions", ''PtrVertexInputAttributeDescription,
		[| #{peek VkPipelineVertexInputStateCreateInfo,
			pVertexAttributeDescriptions} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo,
			pVertexAttributeDescriptions} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
