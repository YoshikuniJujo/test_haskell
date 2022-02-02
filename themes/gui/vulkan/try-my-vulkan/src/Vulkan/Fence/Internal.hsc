{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Fence.Internal where

import Foreign.Storable
import Foreign.C.Struct

import Vulkan.Base
import Vulkan.FenceCreateFlagBits

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkFenceCreateInfo} #{alignment VkFenceCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkFenceCreateInfo, sType}
			p ST.fenceCreateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkFenceCreateInfo, pNext} |],
		[| #{poke VkFenceCreateInfo, pNext} |]),
	("flags", ''FenceCreateFlags, [| #{peek VkFenceCreateInfo, flags} |],
		[| #{poke VkFenceCreateInfo, flags} |]) ]
	[''Show, ''Storable]
