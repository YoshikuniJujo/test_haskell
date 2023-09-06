{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ColorBlendState.Internal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.BlendFactor
import Vulkan.BlendOp
import Vulkan.ColorComponentFlagBits
import Vulkan.LogicOp

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "AttachmentState" #{size VkPipelineColorBlendAttachmentState}
		#{alignment VkPipelineColorBlendAttachmentState} [
	("blendEnable", ''Bool,
		[| (bool32ToBool <$>) .
			#{peek VkPipelineColorBlendAttachmentState,
				blendEnable} |],
		[| \p -> #{poke VkPipelineColorBlendAttachmentState,
			blendEnable} p . boolToBool32 |]),
	("srcColorBlendFactor", ''BlendFactor,
		[| #{peek VkPipelineColorBlendAttachmentState,
			srcColorBlendFactor} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			srcColorBlendFactor} |]),
	("dstColorBlendFactor", ''BlendFactor,
		[| #{peek VkPipelineColorBlendAttachmentState,
			dstColorBlendFactor} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			dstColorBlendFactor} |]),
	("colorBlendOp", ''BlendOp,
		[| #{peek VkPipelineColorBlendAttachmentState, colorBlendOp} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			colorBlendOp} |]),
	("srcAlphaBlendFactor", ''BlendFactor,
		[| #{peek VkPipelineColorBlendAttachmentState,
			srcAlphaBlendFactor} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			srcAlphaBlendFactor} |]),
	("dstAlphaBlendFactor", ''BlendFactor,
		[| #{peek VkPipelineColorBlendAttachmentState,
			dstAlphaBlendFactor} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			dstAlphaBlendFactor} |]),
	("alphaBlendOp", ''BlendOp,
		[| #{peek VkPipelineColorBlendAttachmentState, alphaBlendOp} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			alphaBlendOp} |]),
	("colorWriteMask", ''ColorComponentFlags,
		[| #{peek VkPipelineColorBlendAttachmentState,
			colorWriteMask} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			colorWriteMask} |]) ]
	[''Show, ''Storable]

type PtrAttachmentState = Ptr AttachmentState

enum "CreateFlagBits" ''#{type VkPipelineColorBlendStateCreateFlags}
	[''Show, ''Storable] [("CreateFlagBitsZero", 0)]

type CreateFlags = CreateFlagBits

type ListFloat = [#{type float}]

struct "CreateInfo" #{size VkPipelineColorBlendStateCreateInfo}
		#{alignment VkPipelineColorBlendStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineColorBlendStateCreateInfo, sType} p
			ST.pipelineColorBlendStateCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineColorBlendStateCreateInfo, pNext} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo, pNext} |]),
	("flags", ''CreateFlags,
		[| #{peek VkPipelineColorBlendStateCreateInfo, flags} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo, flags} |]),
	("logicOpEnable", ''Bool32,
		[| #{peek VkPipelineColorBlendStateCreateInfo,
			logicOpEnable} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo,
			logicOpEnable} |]),
	("logicOp", ''LogicOp,
		[| #{peek VkPipelineColorBlendStateCreateInfo, logicOp} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo, logicOp} |]),
	("attachmentCount", ''#{type uint32_t},
		[| #{peek VkPipelineColorBlendStateCreateInfo,
			attachmentCount} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo,
			attachmentCount} |]),
	("pAttachments", ''PtrAttachmentState,
		[| #{peek VkPipelineColorBlendStateCreateInfo, pAttachments} |],
		[| #{poke VkPipelineColorBlendStateCreateInfo,
			pAttachments} |]),
	("blendConstants", ''ListFloat,
		[| \p -> peekArray 4
			$ #{ptr VkPipelineColorBlendStateCreateInfo,
				blendConstants} p |],
		[| \p -> pokeArray
			$ #{ptr VkPipelineColorBlendStateCreateInfo,
				blendConstants} p |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
