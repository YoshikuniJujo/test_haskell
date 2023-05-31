{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Messenger.Core (

	-- * CREATE AND DESTROY

	create, destroy, M, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoMessageSeverity, createInfoMessageType,
	createInfoPfnUserCallback, createInfoPUserData,

	-- * CALLBACK

	wrapCallback, FnCallback, PfnCallback, CallbackData(..),
	pattern CallbackData,
	callbackDataSType, callbackDataPNext, callbackDataFlags,
	callbackDataPMessageIdName, callbackDataMessageIdNumber,
	callbackDataPMessage,
	callbackDataQueueLabelCount, callbackDataPQueueLabels,
	callbackDataCmdBufLabelCount, callbackDataPCmdBufLabels,
	callbackDataObjectCount, callbackDataPObjects

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Core as Instance
import qualified Gpu.Vulkan.Ext.DebugUtils.Core as DU

#include <vulkan/vulkan.h>

data MTag
type M = Ptr MTag

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT}

struct "CallbackData"
		#{size VkDebugUtilsMessengerCallbackDataEXT}
		#{alignment VkDebugUtilsMessengerCallbackDataEXT} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDebugUtilsMessengerCallbackDataEXT, sType}
			p sType |]),
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
	("pQueueLabels", ''DU.PtrLabel,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			pQueueLabels} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			pQueueLabels} |]),
	("cmdBufLabelCount", ''#{type uint32_t},
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			cmdBufLabelCount} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			cmdBufLabelCount} |]),
	("pCmdBufLabels", ''DU.PtrLabel,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			pCmdBufLabels} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			pCmdBufLabels} |]),
	("objectCount", ''#{type uint32_t},
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT,
			objectCount} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT,
			objectCount} |]),
	("pObjects", ''DU.PtrObjectNameInfo,
		[| #{peek VkDebugUtilsMessengerCallbackDataEXT, pObjects} |],
		[| #{poke VkDebugUtilsMessengerCallbackDataEXT, pObjects} |]) ]
	[''Show, ''Storable]

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

create ::
	Instance.I -> Ptr CreateInfo -> Ptr AllocationCallbacks.A ->
	Ptr M -> IO #{type VkResult}
create ist ci ac pm = withCString "vkCreateDebugUtilsMessengerEXT" \nm ->
	c_vkGetInstanceProcAddr ist nm >>= \case
		NullFunPtr -> pure $ #{const VK_ERROR_EXTENSION_NOT_PRESENT}
		pf -> mkFnCreate pf ist ci ac pm

type FnCreate =
	Instance.I -> Ptr CreateInfo -> Ptr AllocationCallbacks.A ->
	Ptr M -> IO #{type VkResult}

foreign import ccall "dynamic" mkFnCreate :: FunPtr FnCreate -> FnCreate

destroy :: Instance.I -> M -> Ptr AllocationCallbacks.A -> IO ()
destroy ist msgr ac = withCString "vkDestroyDebugUtilsMessengerEXT" \nm ->
	c_vkGetInstanceProcAddr ist nm >>= \case
		NullFunPtr -> error "error: extension not present"
		pf -> mkFnDestroy pf ist msgr ac

type FnDestroy = Instance.I -> M -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "dynamic" mkFnDestroy :: FunPtr FnDestroy -> FnDestroy

foreign import ccall "vkGetInstanceProcAddr" c_vkGetInstanceProcAddr ::
	Instance.I -> CString -> IO (FunPtr a)

pattern NullFunPtr :: FunPtr a
pattern NullFunPtr <- ((== nullFunPtr) -> True) where NullFunPtr = nullFunPtr
