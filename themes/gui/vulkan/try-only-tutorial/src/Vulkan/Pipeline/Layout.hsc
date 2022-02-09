{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Layout where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.Device (Device)

import qualified Vulkan.Descriptor.SetLayout as Descriptor.SetLayout
import qualified Vulkan.PushConstant as PushConstant

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO}

struct "CreateInfo" #{size VkPipelineLayoutCreateInfo}
		#{alignment VkPipelineLayoutCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineLayoutCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineLayoutCreateInfo, pNext} |],
		[| #{poke VkPipelineLayoutCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineLayoutCreateFlags},
		[| #{peek VkPipelineLayoutCreateInfo, flags} |],
		[| #{poke VkPipelineLayoutCreateInfo, flags} |]),
	("setLayoutCount", ''#{type uint32_t},
		[| #{peek VkPipelineLayoutCreateInfo, setLayoutCount} |],
		[| #{poke VkPipelineLayoutCreateInfo, setLayoutCount} |]),
	("pSetLayouts", ''Descriptor.SetLayout.PtrSetLayout,
		[| #{peek VkPipelineLayoutCreateInfo, pSetLayouts} |],
		[| #{poke VkPipelineLayoutCreateInfo, pSetLayouts} |]),
	("pushConstantRangeCount", ''#{type uint32_t},
		[| #{peek VkPipelineLayoutCreateInfo,
			pushConstantRangeCount} |],
		[| #{poke VkPipelineLayoutCreateInfo,
			pushConstantRangeCount} |]),
	("pPushConstantRanges", ''PushConstant.PtrRange,
		[| #{peek VkPipelineLayoutCreateInfo, pPushConstantRanges} |],
		[| #{poke VkPipelineLayoutCreateInfo, pPushConstantRanges} |]) ]
	[''Show, ''Storable]

data LayoutTag
type Layout = Ptr LayoutTag

foreign import ccall "vkCreatePipelineLayout" create ::
	Device -> Ptr CreateInfo -> Ptr () -> Ptr Layout -> IO #{type VkResult}

foreign import ccall "vkDestroyPipelineLayout" destroy ::
	Device -> Layout -> Ptr () -> IO ()
