{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Instance.Core (

	-- * CREATE AND DESTROY

	create, destroy, I, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoPApplicationInfo,
	createInfoEnabledLayerCount, createInfoPpEnabledLayerNames,
	createInfoEnabledExtensionCount, createInfoPpEnabledExtensionNames,

	-- * ENUMERATE

	enumerateLayerProperties, enumerateExtensionProperties

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import Gpu.Vulkan.Core

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks

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
	[''Show, ''Storable]

data ITag
type I = Ptr ITag

foreign import ccall "vkCreateInstance" create ::
	Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr I -> IO #{type VkResult}

foreign import ccall "vkDestroyInstance" destroy ::
	I -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "vkEnumerateInstanceExtensionProperties"
	enumerateExtensionProperties ::
	CString -> Ptr #{type uint32_t} -> Ptr ExtensionProperties ->
	IO #{type VkResult}

foreign import ccall "vkEnumerateInstanceLayerProperties"
	enumerateLayerProperties ::
	Ptr #{type uint32_t} -> Ptr LayerProperties -> IO #{type VkResult}
