{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Instance where

import Foreign.Ptr
import Foreign.C.Struct
import Foreign.Storable
import Data.Word
import Data.Int

import Vulkan
import Vulkan.Base

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkInstanceCreateInfo}
		#{alignment VkInstanceCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkInstanceCreateInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkInstanceCreateInfo, pNext} |],
		[| #{poke VkInstanceCreateInfo, pNext} |]),
	("flags", ''#{type VkInstanceCreateFlags},
		[| #{peek VkInstanceCreateInfo, flags} |],
		[| #{poke VkInstanceCreateInfo, flags} |]),
	("pApplicationInfo", ''PtrApplicationInfo,
		[| #{peek VkInstanceCreateInfo, pApplicationInfo} |],
		[| #{poke VkInstanceCreateInfo, pApplicationInfo} |]),
	("enabledLayerCount", ''#{type uint32_t},
		[| #{peek VkInstanceCreateInfo, enabledLayerCount} |],
		[| #{poke VkInstanceCreateInfo, enabledLayerCount} |]),
	("ppEnabledLayerNames", ''PtrCString,
		[| #{peek VkInstanceCreateInfo, ppEnabledLayerNames} |],
		[| #{poke VkInstanceCreateInfo, ppEnabledLayerNames} |]),
	("enabledExtensionCount", ''#{type uint32_t},
		[| #{peek VkInstanceCreateInfo, enabledExtensionCount} |],
		[| #{poke VkInstanceCreateInfo, enabledExtensionCount} |]),
	("ppEnabledExtensionNames", ''PtrCString,
		[| #{peek VkInstanceCreateInfo, ppEnabledExtensionNames} |],
		[| #{poke VkInstanceCreateInfo, ppEnabledExtensionNames} |]) ]
	[''Show]

data InstanceTag
type Instance = Ptr InstanceTag

foreign import ccall "vkCreateInstance" create ::
	Ptr CreateInfo -> Ptr () -> Ptr Instance -> IO #{type VkResult}

foreign import ccall "vkDestroyInstance" destroy :: Instance -> Ptr () -> IO ()
