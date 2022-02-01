{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandPool.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.CommandPoolCreateFlagBits

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkCommandPoolCreateInfo}
		#{alignment VkCommandPoolCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandPoolCreateInfo, sType} p
			ST.commandPoolCreateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkCommandPoolCreateInfo, pNext} |],
		[| #{poke VkCommandPoolCreateInfo, pNext} |]),
	("flags", ''CommandPoolCreateFlags,
		[| #{peek VkCommandPoolCreateInfo, flags} |],
		[| #{poke VkCommandPoolCreateInfo, flags} |]),
	("queueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkCommandPoolCreateInfo, queueFamilyIndex} |],
		[| #{poke VkCommandPoolCreateInfo, queueFamilyIndex} |]) ]
	[''Show, ''Storable]
