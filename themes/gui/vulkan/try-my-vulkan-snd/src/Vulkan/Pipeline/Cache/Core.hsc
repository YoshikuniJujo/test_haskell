{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Cache.Core where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO}

struct "CreateInfo" #{size VkPipelineCacheCreateInfo}
		#{alignment VkPipelineCacheCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineCacheCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineCacheCreateInfo, pNext} |],
		[| #{poke VkPipelineCacheCreateInfo, pNext} |]),
	("flags", ''#{type VkPipelineCacheCreateFlags},
		[| #{peek VkPipelineCacheCreateInfo, flags} |],
		[| #{poke VkPipelineCacheCreateInfo, flags} |]),
	("initialDataSize", ''#{type size_t},
		[| #{peek VkPipelineCacheCreateInfo, initialDataSize} |],
		[| #{poke VkPipelineCacheCreateInfo, initialDataSize} |]),
	("pInitialData", ''PtrVoid,
		[| #{peek VkPipelineCacheCreateInfo, pInitialData} |],
		[| #{poke VkPipelineCacheCreateInfo, pInitialData} |]) ]
	[''Show, ''Storable]
