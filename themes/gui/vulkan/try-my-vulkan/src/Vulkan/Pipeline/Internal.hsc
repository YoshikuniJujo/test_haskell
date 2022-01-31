{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.PipelineCreateFlagBits

import qualified Vulkan.StructureType as ST
import qualified Vulkan.Pipeline.ShaderStage.Internal as ShaderStage.I

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkGraphicsPipelineCreateInfo}
		#{alignment VkGraphicsPipelineCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkGraphicsPipelineCreateInfo, sType} p
			ST.pipelineCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkGraphicsPipelineCreateInfo, pNext} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pNext} |]),
	("flags", ''PipelineCreateFlags,
		[| #{peek VkGraphicsPipelineCreateInfo, flags} |],
		[| #{poke VkGraphicsPipelineCreateInfo, flags} |]),
	("stageCount", ''#{type uint32_t},
		[| #{peek VkGraphicsPipelineCreateInfo, stageCount} |],
		[| #{poke VkGraphicsPipelineCreateInfo, stageCount} |]),
	("pStages", ''ShaderStage.I.CreateInfo,
		[| #{peek VkGraphicsPipelineCreateInfo, pStages} |],
		[| #{poke VkGraphicsPipelineCreateInfo, pStages} |])
	]
	[''Show, ''Storable]
