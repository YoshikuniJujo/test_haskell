{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandPool where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.Device (Device)

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO}

struct "CreateInfo" #{size VkCommandPoolCreateInfo}
		#{alignment VkCommandPoolCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandPoolCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkCommandPoolCreateInfo, pNext} |],
		[| #{poke VkCommandPoolCreateInfo, pNext} |]),
	("flags", ''#{type VkCommandPoolCreateFlags},
		[| #{peek VkCommandPoolCreateInfo, flags} |],
		[| #{poke VkCommandPoolCreateInfo, flags} |]),
	("queueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkCommandPoolCreateInfo, queueFamilyIndex} |],
		[| #{poke VkCommandPoolCreateInfo, queueFamilyIndex} |]) ]
	[''Show, ''Storable]

data CommandPoolTag
type CommandPool = Ptr CommandPoolTag

foreign import ccall "vkCreateCommandPool" create ::
	Device -> Ptr CreateInfo -> Ptr () -> Ptr CommandPool ->
	IO #{type VkResult}

foreign import ccall "vkDestroyCommandPool" destroy ::
	Device -> CommandPool -> Ptr () -> IO ()
