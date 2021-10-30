{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.C.String
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_EXT_debug_utils

createDebugUtilsMessengerEXT ::
	VkInstance -> IO HS_vkCreateDebugUtilsMessengerEXT
createDebugUtilsMessengerEXT i =
	withCString "vkCreateDebugUtilsMessengerEXT" \cs ->
		mkDebugUtilsMessengerEXT . castFunPtr
			<$> vkGetInstanceProcAddr i cs

foreign import ccall "dynamic" mkDebugUtilsMessengerEXT ::
	PFN_vkCreateDebugUtilsMessengerEXT -> HS_vkCreateDebugUtilsMessengerEXT

createDestroyDebugUtilsMessengerEXT ::
	VkInstance -> IO HS_vkDestroyDebugUtilsMessengerEXT
createDestroyDebugUtilsMessengerEXT i =
	withCString "vkDestroyDebugUtilsMessengerEXT" \cs ->
		mkDestroyDebugUtilsMessengerEXT . castFunPtr
			<$> vkGetInstanceProcAddr i cs

foreign import ccall "dynamic" mkDestroyDebugUtilsMessengerEXT ::
	PFN_vkDestroyDebugUtilsMessengerEXT ->
	HS_vkDestroyDebugUtilsMessengerEXT

foreign import ccall "wrapper" wrapDebugUtilsMessengerCallbackEXT ::
	HS_vkDebugUtilsMessengerCallbackEXT ->
	IO PFN_vkDebugUtilsMessengerCallbackEXT
