{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext.Internal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Bits
import Data.Word
import Data.Int

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

enum "DebugUtilsMessageTypeFlagBits"
		''#{type VkDebugUtilsMessageTypeFlagBitsEXT}
		[''Show, ''Eq, ''Bits] [
	("DebugUtilsMessageTypeGeneralBit",
		#{const VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT}),
	("DebugUtilsMessageTypeValidationBit",
		#{const VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT}),
	("DebugUtilsMessageTypePerformanceBit",
		#{const VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT}),
	("DebugUtilsMessageTypeFlagBitsMaxEnum",
		#{const VK_DEBUG_UTILS_MESSAGE_TYPE_FLAG_BITS_MAX_ENUM_EXT}) ]

structureTypeDebugUtilsMessengerCallbackData :: #{type VkStructureType}
structureTypeDebugUtilsMessengerCallbackData =
	#{const VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT}

enum "DebugUtilsMessengerCallbackDataFlags"
	''#{type VkDebugUtilsMessengerCallbackDataFlagsEXT}
	[''Show, ''Storable] []

type ListFloat = [Float]

struct "DebugUtilsLabelRaw"
		#{size VkDebugUtilsLabelEXT} #{alignment VkDebugUtilsLabelEXT} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDebugUtilsLabelEXT, sType} p
			(#{const VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkDebugUtilsLabelEXT, pNext} |],
		[| #{poke VkDebugUtilsLabelEXT, pNext} |]),
	("pLabelName", ''CString,
		[| #{peek VkDebugUtilsLabelEXT, pLabelName} |],
		[| #{poke VkDebugUtilsLabelEXT, pLabelName} |]),
	("color", ''ListFloat,
		[| peekArray 4 . #{ptr VkDebugUtilsLabelEXT, color} |],
		[| pokeArray . #{ptr VkDebugUtilsLabelEXT, color} |]) ]
	[''Show]

type PtrDebugUtilsLabelRaw = Ptr DebugUtilsLabelRaw

struct "DebugUtilsMessengerCallbackDataRaw"
		#{size VkDebugUtilsMessengerCallbackDataEXT}
		#{alignment VkDebugUtilsMessengerCallbackDataEXT} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDebugUtilsMessengerCallbackDataEXT, sType}
			p structureTypeDebugUtilsMessengerCallbackData |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT, pNext} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT, pNext} |]),
	("flags", ''DebugUtilsMessengerCallbackDataFlags,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT, flags} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT, flags} |]),
	("pMessageIdName", ''CString,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			pMessageIdName} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			pMessageIdName} |]),
	("messageIdNumber", ''#{type int32_t},
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			messageIdNumber} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			messageIdNumber} |]),
	("pMessage", ''CString,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT, pMessage} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT, pMessage} |]),
	("queueLabelCount", ''#{type uint32_t},
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			queueLabelCount} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			queueLabelCount} |]),
	("pQueueLabels", ''PtrDebugUtilsLabelRaw,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			pQueueLabels} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			pQueueLabels} |])
	]
	[''Show]

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
