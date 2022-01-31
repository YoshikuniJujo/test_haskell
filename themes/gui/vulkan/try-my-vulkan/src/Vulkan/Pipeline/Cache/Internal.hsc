{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Cache.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.PipelineCacheCreateFlagBits

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkPipelineCacheCreateInfo}
		#{alignment VkPipelineCacheCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineCacheCreateInfo, sType} p
			ST.pipelineCacheCreateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkPipelineCacheCreateInfo, pNext} |],
		[| #{poke VkPipelineCacheCreateInfo, pNext} |]),
	("flags", ''PipelineCacheCreateFlags,
		[| #{peek VkPipelineCacheCreateInfo, flags} |],
		[| #{poke VkPipelineCacheCreateInfo, flags} |]),
	("initialDataSize", ''#{type size_t},
		[| #{peek VkPipelineCacheCreateInfo, initialDataSize} |],
		[| #{poke VkPipelineCacheCreateInfo, initialDataSize} |]),
	("pInitialData", ''PtrVoid,
		[| #{peek VkPipelineCacheCreateInfo, pInitialData} |],
		[| #{poke VkPipelineCacheCreateInfo, pInitialData} |]) ]
	[''Show, ''Storable]
