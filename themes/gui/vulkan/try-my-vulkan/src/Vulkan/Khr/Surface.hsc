{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.Instance
import Vulkan.AllocationCallbacks
import Vulkan.PhysicalDevice

#include <vulkan/vulkan.h>

import qualified Vulkan.AllocationCallbacks.Internal as I

newtype Surface = Surface (Ptr Surface) deriving (Show, Storable)

destroySurface :: Pointable n =>
	Instance -> Surface -> Maybe (AllocationCallbacks n) -> IO ()
destroySurface ist sfc mac = ($ pure) $ runContT do
	piac <- case mac of
		Nothing -> pure NullPtr
		Just ac -> ContT $ withAllocationCallbacksPtr ac
	lift $ c_vkDestroySurfaceKHR ist sfc piac

foreign import ccall "vkDestroySurfaceKHR" c_vkDestroySurfaceKHR ::
	Instance -> Surface -> Ptr I.AllocationCallbacks -> IO ()

getPhysicalDeviceSurfaceSupport ::
	PhysicalDevice -> #{type uint32_t} -> Surface -> IO Bool32
getPhysicalDeviceSurfaceSupport phdv qfi sfc = ($ pure) $ runContT do
	pspt <- ContT alloca
	lift do	r <- c_vkGetPhysicalDeviceSurfaceSupportKHR phdv qfi sfc pspt
		throwUnlessSuccess r
		peek pspt

foreign import ccall "vkGetPhysicalDeviceSurfaceSupportKHR"
	c_vkGetPhysicalDeviceSurfaceSupportKHR ::
	PhysicalDevice -> #{type uint32_t} -> Surface -> Ptr Bool32 -> IO Result

-- VkSurfaceCapabilitiesKHR
