{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Shader.Module.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base

import qualified Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Vulkan.Device.Core as Device

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

data ModuleTag
type Module = Ptr ModuleTag

foreign import ccall "vkCreateShaderModule" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr Module -> IO #{type VkResult}

foreign import ccall "vkDestroyShaderModule" destroy ::
	Device.D -> Module -> Ptr AllocationCallbacks.A -> IO ()
