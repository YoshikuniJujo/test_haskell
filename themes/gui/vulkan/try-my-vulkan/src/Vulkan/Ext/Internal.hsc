{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan
import Vulkan.Internal

#include <vulkan/vulkan.h>

struct "DebugUtilsMessengerCreateInfo"
		#{size VkDebugUtilsMessengerCreateInfoEXT}
		#{alignment VkDebugUtilsMessengerCreateInfoEXT} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDebugUtilsMessengerCreateInfoEXT, sType} p
			(#{const VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT, pNext} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT, pNext} |]),
	("flags", ''#{type VkDebugUtilsMessengerCreateFlagsEXT},
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT, flags} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT, flags} |]),
	("messageSeverity", ''#{type VkDebugUtilsMessageSeverityFlagsEXT},
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT,
			messageSeverity} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT,
			messageSeverity} |]),
	("messageType", ''#{type VkDebugUtilsMessageTypeFlagsEXT},
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT, messageType} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT, messageType} |])
	]
	[''Show]
