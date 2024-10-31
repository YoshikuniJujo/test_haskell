{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineLayout.Core (

	-- * CREATE AND DESTROY

	create, destroy, P, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoSetLayoutCount, createInfoPSetLayouts,
	createInfoPushConstantRangeCount, createInfoPPushConstantRanges

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.DescriptorSetLayout.Core as DescriptorSet.Layout
import qualified Gpu.Vulkan.PushConstant.Core as PushConstant

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
	("pSetLayouts", ''DescriptorSet.Layout.PtrD,
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

data PTag
type P = Ptr PTag

foreign import ccall "vkCreatePipelineLayout" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr P -> IO #{type VkResult}

foreign import ccall "vkDestroyPipelineLayout" destroy ::
	Device.D -> P -> Ptr AllocationCallbacks.A -> IO ()
