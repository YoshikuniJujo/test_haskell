{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext.Internal where

import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Bits
import Data.Word

import Vulkan
import Vulkan.Internal

#include <vulkan/vulkan.h>

enum "DebugUtilsMessageSeverityFlagBits"
		''#{type VkDebugUtilsMessageSeverityFlagBitsEXT}
		[''Show, ''Eq, ''Bits] [
	("DebugUtilsMessageSeverityVerboseBit",
		#{const VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT}),
	("DebugUtilsMessageSeverityInfoBit",
		#{const VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT}),
	("DebugUtilsMessageSeverityWarningBit",
		#{const VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT}),
	("DebugUtilsMessageSeverityErrorBit",
		#{const VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT}),
	("DebugUtilsMessageSeverityFlagBitsMaxEnum",
		#{const VK_DEBUG_UTILS_MESSAGE_SEVERITY_FLAG_BITS_MAX_ENUM_EXT})
	]

{-
type FN_DebugUtilsMessengerCallback a =
	#{type VkDebugUtilsMessageSeverityFlagBitsEXT} ->
	#{type VkDebugUtilsMessageTypeFlagsEXT} ->
	Ptr #{type VkDebugUtilsMessengerCallbackDataEXT} ->
	Ptr a -> IO #{type VkBool32}
	-}

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
