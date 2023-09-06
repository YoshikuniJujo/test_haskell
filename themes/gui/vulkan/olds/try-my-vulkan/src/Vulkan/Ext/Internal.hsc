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
import Vulkan.Instance
import Vulkan.Base
import Vulkan.Exception
import qualified Vulkan.AllocationCallbacks.Internal as I

#include <vulkan/vulkan.h>

enum "DebugUtilsMessageSeverityFlagBits"
		''#{type VkDebugUtilsMessageSeverityFlagBitsEXT}
		[''Show, ''Eq, ''Bits, ''Storable] [
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
		[''Show, ''Eq, ''Bits, ''Storable] [
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
	''#{type VkDebugUtilsMessengerCallbackDataFlagsEXT} [''Show, ''Storable] [
	("DebugUtilsMessengerCallbackDataFlagsZero", 0) ]

struct "DebugUtilsLabel"
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
	("color", ''ListCFloat,
		[| peekArray 4 . #{ptr VkDebugUtilsLabelEXT, color} |],
		[| pokeArray . #{ptr VkDebugUtilsLabelEXT, color} |]) ]
	[''Show, ''Storable]

type PtrDebugUtilsLabel = Ptr DebugUtilsLabel

structureTypeDebugUtilsObjectNameInfo :: #{type VkStructureType}
structureTypeDebugUtilsObjectNameInfo =
	#{const VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT}

struct "DebugUtilsObjectNameInfo"
		#{size VkDebugUtilsObjectNameInfoEXT}
		#{alignment VkDebugUtilsObjectNameInfoEXT} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDebugUtilsObjectNameInfoEXT, sType} p
			structureTypeDebugUtilsObjectNameInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDebugUtilsObjectNameInfoEXT, pNext} |],
		[| #{poke VkDebugUtilsObjectNameInfoEXT, pNext} |]),
	("objectType", ''ObjectType,
		[| #{peek VkDebugUtilsObjectNameInfoEXT, objectType} |],
		[| #{poke VkDebugUtilsObjectNameInfoEXT, objectType} |]),
	("objectHandle", ''#{type uint64_t},
		[| #{peek VkDebugUtilsObjectNameInfoEXT, objectHandle} |],
		[| #{poke VkDebugUtilsObjectNameInfoEXT, objectHandle} |]),
	("pObjectName", ''CString,
		[| #{peek VkDebugUtilsObjectNameInfoEXT, pObjectName} |],
		[| #{poke VkDebugUtilsObjectNameInfoEXT, pObjectName} |]) ]
	[''Show, ''Storable]

type PtrDebugUtilsObjectNameInfo = Ptr DebugUtilsObjectNameInfo

struct "DebugUtilsMessengerCallbackData"
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
	("pQueueLabels", ''PtrDebugUtilsLabel,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			pQueueLabels} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			pQueueLabels} |]),
	("cmdBufLabelCount", ''#{type uint32_t},
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			cmdBufLabelCount} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			cmdBufLabelCount} |]),
	("pCmdBufLabels", ''PtrDebugUtilsLabel,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			pCmdBufLabels} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			pCmdBufLabels} |]),
	("objectCount", ''#{type uint32_t},
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			objectCount} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			objectCount} |]),
	("pObjects", ''PtrDebugUtilsObjectNameInfo,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT, pObjects} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT, pObjects} |]) ]
	[''Show]

type FnDebugUtilsMessengerCallback =
	DebugUtilsMessageSeverityFlagBits -> DebugUtilsMessageTypeFlagBits ->
	Ptr DebugUtilsMessengerCallbackData -> Ptr () -> IO Bool32

type FunPtrFnDebugUtilsMessengerCallback = FunPtr FnDebugUtilsMessengerCallback

enum "DebugUtilsMessengerCreateFlags"
	''#{type VkDebugUtilsMessengerCreateFlagsEXT} [''Show, ''Storable] [
	("DebugUtilsMessengerCreateFlagsZero", 0) ]

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
	("flags", ''DebugUtilsMessengerCreateFlags,
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT, flags} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT, flags} |]),
	("messageSeverity", ''DebugUtilsMessageSeverityFlagBits,
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT,
			messageSeverity} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT,
			messageSeverity} |]),
	("messageType", ''DebugUtilsMessageTypeFlagBits,
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT, messageType} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT, messageType} |]),
	("pfnUserCallback", ''FunPtrFnDebugUtilsMessengerCallback,
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT, pfnUserCallback} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT, pfnUserCallback} |]),
	("pUserData", ''PtrVoid,
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT, pUserData} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT, pUserData} |]) ]
	[''Show, ''Storable]

foreign import ccall "wrapper" wrapFnDebugUtilsMessengerCallback ::
	FnDebugUtilsMessengerCallback -> IO FunPtrFnDebugUtilsMessengerCallback

newtype DebugUtilsMessenger = DebugUtilsMessenger (Ptr DebugUtilsMessenger)
	deriving Show

type FnCreateDebugUtilsMessenger =
	Ptr Instance -> Ptr DebugUtilsMessengerCreateInfo ->
	Ptr I.AllocationCallbacks -> Ptr (Ptr DebugUtilsMessenger) -> IO Result

foreign import ccall "vkGetInstanceProcAddr" c_vkGetInstanceProcAddr ::
	Ptr Instance -> CString -> IO (FunPtr a)

foreign import ccall "dynamic" mkFnCreateDebugUtilsMessenger ::
	FunPtr FnCreateDebugUtilsMessenger -> FnCreateDebugUtilsMessenger

type FnDestroyDebugUtilsMessenger = Ptr Instance ->
	Ptr DebugUtilsMessenger -> Ptr I.AllocationCallbacks -> IO ()

foreign import ccall "dynamic" mkFnDestroyDebugUtilsMessenger ::
	FunPtr FnDestroyDebugUtilsMessenger -> FnDestroyDebugUtilsMessenger
