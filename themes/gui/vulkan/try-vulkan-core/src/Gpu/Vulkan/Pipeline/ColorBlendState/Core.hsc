{-# LANGUAGE TemplateHaskell #-} {-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ColorBlendState.Core (

	-- * CREATE INFO

	CreateInfo, PtrCreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoLogicOpEnable, createInfoLogicOp,
	createInfoAttachmentCount, createInfoPAttachments,
	createInfoBlendConstants

	) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment.Core as ColorBlendAttachment

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO}

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
	("pAttachments", ''ColorBlendAttachment.PtrState,
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
