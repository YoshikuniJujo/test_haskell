{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Compute.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

data CTag
type C = Ptr CTag

stype :: #{type VkStructureType}
stype = #{const VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO}

struct "CreateInfo" #{size VkComputePipelineCreateInfo}
		#{alignment VkComputePipelineCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkComputePipelineCreateInfo, sType}
			p stype |]),
	("pNext", ''PtrVoid,
		[| #{peek VkComputePipelineCreateInfo, pNext} |],
		[| #{poke VkComputePipelineCreateInfo, pNext} |])
	]
	[''Show, ''Storable]
