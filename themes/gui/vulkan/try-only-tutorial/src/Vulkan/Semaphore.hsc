{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Semaphore where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan (Semaphore)
import Vulkan.Base
import Vulkan.Device (Device)

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO}

struct "CreateInfo" #{size VkSemaphoreCreateInfo}
		#{alignment VkSemaphoreCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSemaphoreCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkSemaphoreCreateInfo, pNext} |],
		[| #{poke VkSemaphoreCreateInfo, pNext} |]),
	("flags", ''#{type VkSemaphoreCreateFlags},
		[| #{peek VkSemaphoreCreateInfo, flags} |],
		[| #{poke VkSemaphoreCreateInfo, flags} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateSemaphore" create ::
	Device -> Ptr CreateInfo -> Ptr () -> Ptr Semaphore ->
	IO #{type VkResult}

foreign import ccall "vkDestroySemaphore" destroy ::
	Device -> Semaphore -> Ptr () -> IO ()
