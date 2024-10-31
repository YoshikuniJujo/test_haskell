{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ColorBlendAttachment.Core (

	-- * STATE

	State, PtrState, pattern State,
	stateBlendEnable,
	stateSrcColorBlendFactor, stateDstColorBlendFactor, stateColorBlendOp,
	stateSrcAlphaBlendFactor, stateDstAlphaBlendFactor, stateAlphaBlendOp,
	stateColorWriteMask

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

struct "State" #{size VkPipelineColorBlendAttachmentState}
		#{alignment VkPipelineColorBlendAttachmentState} [
	("blendEnable", ''#{type VkBool32},
		[| #{peek VkPipelineColorBlendAttachmentState, blendEnable} |],
		[| #{poke VkPipelineColorBlendAttachmentState, blendEnable} |]),
	("srcColorBlendFactor", ''#{type VkBlendFactor},
		[| #{peek VkPipelineColorBlendAttachmentState,
			srcColorBlendFactor} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			srcColorBlendFactor} |]),
	("dstColorBlendFactor", ''#{type VkBlendFactor},
		[| #{peek VkPipelineColorBlendAttachmentState,
			dstColorBlendFactor} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			dstColorBlendFactor} |]),
	("colorBlendOp", ''#{type VkBlendOp},
		[| #{peek VkPipelineColorBlendAttachmentState, colorBlendOp} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			colorBlendOp} |]),
	("srcAlphaBlendFactor", ''#{type VkBlendFactor},
		[| #{peek VkPipelineColorBlendAttachmentState,
			srcAlphaBlendFactor} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			srcAlphaBlendFactor} |]),
	("dstAlphaBlendFactor", ''#{type VkBlendFactor},
		[| #{peek VkPipelineColorBlendAttachmentState,
			dstAlphaBlendFactor} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			dstAlphaBlendFactor} |]),
	("alphaBlendOp", ''#{type VkBlendOp},
		[| #{peek VkPipelineColorBlendAttachmentState, alphaBlendOp} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			alphaBlendOp} |]),
	("colorWriteMask", ''#{type VkColorComponentFlags},
		[| #{peek VkPipelineColorBlendAttachmentState,
			colorWriteMask} |],
		[| #{poke VkPipelineColorBlendAttachmentState,
			colorWriteMask} |]) ]
	[''Show, ''Storable]

type PtrState = Ptr State
