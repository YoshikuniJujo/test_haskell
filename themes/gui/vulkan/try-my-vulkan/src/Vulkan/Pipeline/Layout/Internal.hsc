{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Layout.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.Descriptor.SetLayout (PtrDescriptorSetLayout)
import Vulkan.ShaderStageFlagBits

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

enum "CreateFlagBits" ''#{type VkPipelineLayoutCreateFlags}
	[''Show, ''Storable] [("CreateFlagBitsZero", 0)]

type CreateFlags = CreateFlagBits

struct "PushConstantRange" #{size VkPushConstantRange}
		#{alignment VkPushConstantRange} [
	("stageFlags", ''ShaderStageFlags,
		[| #{peek VkPushConstantRange, stageFlags} |],
		[| #{poke VkPushConstantRange, stageFlags} |]),
	("offset", ''#{type uint32_t},
		[| #{peek VkPushConstantRange, offset} |],
		[| #{poke VkPushConstantRange, offset} |]),
	("size", ''#{type uint32_t},
		[| #{peek VkPushConstantRange, size} |],
		[| #{poke VkPushConstantRange, size} |]) ]
	[''Show, ''Storable]

type PtrPushConstantRange = Ptr PushConstantRange

struct "CreateInfo" #{size VkPipelineLayoutCreateInfo}
		#{alignment VkPipelineLayoutCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineLayoutCreateInfo, sType} p
			ST.pipelineLayoutCreateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkPipelineLayoutCreateInfo, pNext} |],
		[| #{poke VkPipelineLayoutCreateInfo, pNext} |]),
	("flags", ''CreateFlags,
		[| #{peek VkPipelineLayoutCreateInfo, flags} |],
		[| #{poke VkPipelineLayoutCreateInfo, flags} |]),
	("setLayoutCount", ''#{type uint32_t},
		[| #{peek VkPipelineLayoutCreateInfo, setLayoutCount} |],
		[| #{poke VkPipelineLayoutCreateInfo, setLayoutCount} |]),
	("pSetLayouts", ''PtrDescriptorSetLayout,
		[| #{peek VkPipelineLayoutCreateInfo, pSetLayouts} |],
		[| #{poke VkPipelineLayoutCreateInfo, pSetLayouts} |]),
	("pushConstantRangeCount", ''#{type uint32_t},
		[| #{peek VkPipelineLayoutCreateInfo,
			pushConstantRangeCount} |],
		[| #{poke VkPipelineLayoutCreateInfo,
			pushConstantRangeCount} |]),
	("pPushConstantRanges", ''PtrPushConstantRange,
		[| #{peek VkPipelineLayoutCreateInfo, pPushConstantRanges} |],
		[| #{poke VkPipelineLayoutCreateInfo, pPushConstantRanges} |]) ]
	[''Show, ''Storable]
