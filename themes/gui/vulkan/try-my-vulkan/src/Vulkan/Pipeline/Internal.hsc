{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Internal where

import Foreign.Storable
import Foreign.C.Struct

import Vulkan.Base
import Vulkan.PipelineCreateFlagBits

import qualified Vulkan.StructureType as ST

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
		[| #{poke VkGraphicsPipelineCreateInfo, flags} |])
	]
	[''Show, ''Storable]
