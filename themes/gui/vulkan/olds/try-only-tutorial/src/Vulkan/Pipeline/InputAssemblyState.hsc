{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.InputAssemblyState where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO}

struct "CreateInfo" #{size VkPipelineInputAssemblyStateCreateInfo}
		#{alignment VkPipelineInputAssemblyStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineInputAssemblyStateCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineInputAssemblyStateCreateInfo, pNext} |],
		[| #{poke VkPipelineInputAssemblyStateCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineInputAssemblyStateCreateFlags},
		[| #{peek VkPipelineInputAssemblyStateCreateInfo, flags} |],
		[| #{poke VkPipelineInputAssemblyStateCreateInfo, flags} |]),
	("topology", ''#{type VkPrimitiveTopology},
		[| #{peek VkPipelineInputAssemblyStateCreateInfo, topology} |],
		[| #{poke VkPipelineInputAssemblyStateCreateInfo, topology} |]),
	("primitiveRestartEnable", ''#{type VkBool32},
		[| #{peek VkPipelineInputAssemblyStateCreateInfo,
			primitiveRestartEnable} |],
		[| #{poke VkPipelineInputAssemblyStateCreateInfo,
			primitiveRestartEnable} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
