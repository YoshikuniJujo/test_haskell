{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Shader.Module where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO}

struct "CreateInfo" #{size VkShaderModuleCreateInfo}
		#{alignment VkShaderModuleCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkShaderModuleCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid, [| #{peek VkShaderModuleCreateInfo, pNext} |],
		[| #{poke VkShaderModuleCreateInfo, pNext} |]),
	("flags", ''#{type VkShaderModuleCreateFlags},
		[| #{peek VkShaderModuleCreateInfo, flags} |],
		[| #{poke VkShaderModuleCreateInfo, flags} |]),
	("codeSize", ''#{type size_t},
		[| #{peek VkShaderModuleCreateInfo, codeSize} |],
		[| #{poke VkShaderModuleCreateInfo, codeSize} |]),
	("pCode", ''PtrUint32T,
		[| #{peek VkShaderModuleCreateInfo, pCode} |],
		[| #{poke VkShaderModuleCreateInfo, pCode} |]) ]
	[''Show, ''Storable]
