{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Shader.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

enum "ShaderModuleCreateFlags" ''#{type VkShaderModuleCreateFlags}
		[''Show, ''Storable] [
	("ShaderModuleCreateFlagsZero", 0) ]

struct "ShaderModuleCreateInfo" #{size VkShaderModuleCreateInfo}
		#{alignment VkShaderModuleCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkShaderModuleCreateInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkShaderModuleCreateInfo, pNext} |],
		[| #{poke VkShaderModuleCreateInfo, pNext} |]),
	("flags", ''ShaderModuleCreateFlags,
		[| #{peek VkShaderModuleCreateInfo, flags} |],
		[| #{poke VkShaderModuleCreateInfo, flags} |]),
	("codeSize", ''#{type size_t},
		[| #{peek VkShaderModuleCreateInfo, codeSize} |],
		[| #{poke VkShaderModuleCreateInfo, codeSize} |]),
	("pCode", ''PtrUint32T,
		[| #{peek VkShaderModuleCreateInfo, pCode} |],
		[| #{poke VkShaderModuleCreateInfo, pCode} |]) ]
	[''Show]
