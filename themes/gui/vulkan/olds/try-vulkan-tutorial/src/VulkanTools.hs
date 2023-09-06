{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanTools where

import qualified Vulkan as V

setFnUserCallback ::
	V.DebugUtilsMessengerCreateInfoEXT ->
	V.FN_vkDebugUtilsMessengerCallbackEXT ->
	IO V.DebugUtilsMessengerCreateInfoEXT
setFnUserCallback ci f = do
	pf <- wrapFnUserCallback f
	pure $ (ci :: V.DebugUtilsMessengerCreateInfoEXT)
		{ V.pfnUserCallback = pf :: V.PFN_vkDebugUtilsMessengerCallbackEXT }

foreign import ccall "wrapper" wrapFnUserCallback ::
	V.FN_vkDebugUtilsMessengerCallbackEXT ->
	IO (V.PFN_vkDebugUtilsMessengerCallbackEXT)
