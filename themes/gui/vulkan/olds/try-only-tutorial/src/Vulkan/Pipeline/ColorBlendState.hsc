{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ColorBlendState where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

import qualified Vulkan.Pipeline.ColorBlendAttachmentState as
	ColorBlendAttachmentState

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO}

type ListFloat = [#{type float}]

struct "CreateInfo" #{size VkPipelineColorBlendStateCreateInfo}
		#{alignment VkPipelineColorBlendStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineColorBlendStateCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineColorBlendStateCreateInfo, pNext} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineColorBlendStateCreateFlags},
		[| #{peek VkPipelineColorBlendStateCreateInfo, flags} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo, flags} |]),
	("logicOpEnable", ''#{type VkBool32},
		[| #{peek VkPipelineColorBlendStateCreateInfo,
			logicOpEnable} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo,
			logicOpEnable} |]),
	("logicOp", ''#{type VkLogicOp},
		[| #{peek VkPipelineColorBlendStateCreateInfo, logicOp} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo, logicOp} |]),
	("attachmentCount", ''#{type uint32_t},
		[| #{peek VkPipelineColorBlendStateCreateInfo,
			attachmentCount} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo,
			attachmentCount} |]),
	("pAttachments", ''ColorBlendAttachmentState.PtrState,
		[| #{peek VkPipelineColorBlendStateCreateInfo, pAttachments} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo,
			pAttachments} |]),
	("blendConstants", ''ListFloat,
		[| \p -> peekArray 4 (#{ptr VkPipelineColorBlendStateCreateInfo,
			blendConstants} p) |],
		[| \p bcs -> pokeArray (#{ptr VkPipelineColorBlendStateCreateInfo,
			blendConstants} p) $ take 4 bcs |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
