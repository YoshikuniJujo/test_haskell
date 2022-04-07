{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Fence.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

data FTag
type F = Ptr FTag

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_FENCE_CREATE_INFO}

struct "CreateInfo" #{size VkFenceCreateInfo} #{alignment VkFenceCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkFenceCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkFenceCreateInfo, pNext} |],
		[| #{poke VkFenceCreateInfo, pNext} |]),
	("flags", ''#{type VkFenceCreateFlags},
		[| #{peek VkFenceCreateInfo, flags} |],
		[| #{poke VkFenceCreateInfo, flags} |]) ]
	[''Show, ''Storable]
