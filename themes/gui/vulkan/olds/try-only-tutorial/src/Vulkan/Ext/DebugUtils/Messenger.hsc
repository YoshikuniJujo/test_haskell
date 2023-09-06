{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext.DebugUtils.Messenger where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.Instance (Instance)

#include <vulkan/vulkan.h>

data MessengerTag
type Messenger = Ptr MessengerTag

type PtrDebugUtilsLabel = Ptr ()
type PtrDebugUtilsObjectNameInfo = Ptr ()

structureTypeDebugUtilsMessengerCallbackData :: #{type VkStructureType}
structureTypeDebugUtilsMessengerCallbackData =
	#{const VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT}

struct "CallbackData"
		#{size VkDebugUtilsMessengerCallbackDataEXT}
		#{alignment VkDebugUtilsMessengerCallbackDataEXT} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDebugUtilsMessengerCallbackDataEXT, sType}
			p structureTypeDebugUtilsMessengerCallbackData |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT, pNext} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT, pNext} |]),
	("flags", ''#{type VkDebugUtilsMessengerCallbackDataFlagsEXT},
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
	[''Show, ''Storable]

copyCallbackData :: Ptr CallbackData -> IO CallbackData
copyCallbackData psrc = do
	pdst <- malloc
	copyBytes pdst psrc $ sizeOf @CallbackData undefined
	CallbackData_ <$> newForeignPtr pdst (free pdst)

type FnCallback =
	#{type VkDebugUtilsMessageSeverityFlagBitsEXT} ->
	#{type VkDebugUtilsMessageTypeFlagsEXT} ->
	Ptr CallbackData -> Ptr () -> IO #{type VkBool32}

type PfnCallback = FunPtr FnCallback

foreign import ccall "wrapper" wrapCallback :: FnCallback -> IO PfnCallback

struct "CreateInfo"
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
	("messageSeverity", ''#{type VkDebugUtilsMessageSeverityFlagBitsEXT},
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT,
			messageSeverity} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT,
			messageSeverity} |]),
	("messageType", ''#{type VkDebugUtilsMessageTypeFlagBitsEXT},
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT, messageType} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT, messageType} |]),
	("pfnUserCallback", ''PfnCallback,
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT, pfnUserCallback} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT, pfnUserCallback} |]),
	("pUserData", ''PtrVoid,
		[| #{peek VkDebugUtilsMessengerCreateInfoEXT, pUserData} |],
		[| #{poke VkDebugUtilsMessengerCreateInfoEXT, pUserData} |]) ]
	[''Show, ''Storable]

severityVerboseBit, severityWarningBit, severityErrorBit ::
	#{type VkDebugUtilsMessageSeverityFlagBitsEXT}
severityVerboseBit = #{const VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT}
severityWarningBit = #{const VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT}
severityErrorBit = #{const VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT}

typeGeneralBit, typeValidationBit, typePerformanceBit ::
	#{type VkDebugUtilsMessageTypeFlagBitsEXT}
typeGeneralBit = #{const VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT}
typeValidationBit = #{const VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT}
typePerformanceBit = #{const VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT}

create :: Instance -> Ptr CreateInfo -> Ptr () -> Ptr Messenger ->
	IO #{type VkResult}
create ist ci ac pm = withCString "vkCreateDebugUtilsMessengerEXT" \nm ->
	c_vkGetInstanceProcAddr ist nm >>= \case
		NullFunPtr -> pure $ #{const VK_ERROR_EXTENSION_NOT_PRESENT}
		pf -> mkFnCreate pf ist ci ac pm

type FnCreate = Instance -> Ptr CreateInfo -> Ptr () -> Ptr Messenger ->
	IO #{type VkResult}

foreign import ccall "dynamic" mkFnCreate :: FunPtr FnCreate -> FnCreate

destroy :: Instance -> Messenger -> Ptr () -> IO ()
destroy ist msgr ac = withCString "vkDestroyDebugUtilsMessengerEXT" \nm ->
	c_vkGetInstanceProcAddr ist nm >>= \case
		NullFunPtr -> error "error: extension not present"
		pf -> mkFnDestroy pf ist msgr ac

type FnDestroy = Instance -> Messenger -> Ptr () -> IO ()

foreign import ccall "dynamic" mkFnDestroy :: FunPtr FnDestroy -> FnDestroy

foreign import ccall "vkGetInstanceProcAddr" c_vkGetInstanceProcAddr ::
	Instance -> CString -> IO (FunPtr a)
