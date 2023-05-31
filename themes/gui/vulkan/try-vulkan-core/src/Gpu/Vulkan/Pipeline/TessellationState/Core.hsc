{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.TessellationState.Core (

	-- * CREATE INFO

	CreateInfo, PtrCreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoPatchControlPoints

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO}

struct "CreateInfo" #{size VkPipelineTessellationStateCreateInfo}
		#{alignment VkPipelineTessellationStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineTessellationStateCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineTessellationStateCreateInfo, pNext} |],
		[| #{poke VkPipelineTessellationStateCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineTessellationStateCreateFlags},
		[| #{peek VkPipelineTessellationStateCreateInfo, flags} |],
		[| #{poke VkPipelineTessellationStateCreateInfo, flags} |]),
	("patchControlPoints", ''#{type uint32_t},
		[| #{peek VkPipelineTessellationStateCreateInfo,
			patchControlPoints} |],
		[| #{poke VkPipelineTessellationStateCreateInfo,
			patchControlPoints} |]) ]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
