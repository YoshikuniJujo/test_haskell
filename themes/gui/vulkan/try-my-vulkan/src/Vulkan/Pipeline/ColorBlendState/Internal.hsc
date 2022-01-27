{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.ColorBlendState.Internal where

import Foreign.Storable
import Foreign.C.Struct

import Vulkan.Base
import Vulkan.BlendFactor
import Vulkan.BlendOp
import Vulkan.ColorComponentFlagBits

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
