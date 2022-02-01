{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer.Internal where

import Foreign.Storable
import Foreign.C.Struct

import Vulkan.Base

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "AllocateInfo" #{size VkCommandBufferAllocateInfo}
		#{alignment VkCommandBufferAllocateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandBufferAllocateInfo, sType}
			p ST.commandBufferAllocateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkCommandBufferAllocateInfo, pNext} |],
		[| #{poke VkCommandBufferAllocateInfo, pNext} |])
	]
	[''Show, ''Storable]
