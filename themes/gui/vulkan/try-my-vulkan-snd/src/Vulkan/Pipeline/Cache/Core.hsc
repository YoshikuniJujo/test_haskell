{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Cache.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.Exception.Enum

import qualified Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Vulkan.Device.Core as Device

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

foreign import ccall "VkCreatePipelineCache" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr C ->
	IO Result

foreign import ccall "VkDestroyPipelineCache" destroy ::
	Device.D -> C -> Ptr AllocationCallbacks.A -> IO ()
