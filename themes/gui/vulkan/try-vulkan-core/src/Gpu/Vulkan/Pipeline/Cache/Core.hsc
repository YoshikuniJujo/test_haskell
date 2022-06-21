{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Cache.Core where

import Foreign.Ptr
import Foreign.Ptr.Synonyms
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device

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

data CTag
type C = Ptr CTag

foreign import ccall "vkCreatePipelineCache" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr C ->
	IO #{type VkResult}

foreign import ccall "vkDestroyPipelineCache" destroy ::
	Device.D -> C -> Ptr AllocationCallbacks.A -> IO ()
