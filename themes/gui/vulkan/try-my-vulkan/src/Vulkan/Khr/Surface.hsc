{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Enum
import Control.Monad.Cont
import Data.Bits
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

enum "SurfaceTransformFlagBits" ''#{type VkSurfaceTransformFlagBitsKHR}
		[''Show, ''Eq, ''Bits, ''Storable] [
	("SurfaceTransformIdentityBit",
		#{const VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR}),
	("SurfaceTransformRotate90Bit",
		#{const VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR}),
	("SurfaceTransformRotate180Bit",
		#{const VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR}),
	("SurfaceTransformRotate270Bit",
		#{const VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR}),
	("SurfaceTransformHorizontalMirrorBit",
		#{const VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR}),
	("SurfaceTransformHorizontalMirrorRotate90Bit", #{const
		VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR}),
	("SurfaceTransformHorizontalMirrorRotate180Bit", #{const
		VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR}),
	("SurfaceTransformHorizontalMirrorRotate270Bit", #{const
		VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR}),
	("SurfaceTransformInheritBit",
		#{const VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR}),
	("SurfaceTransformFlagBitsMaxEnum",
		#{const VK_SURFACE_TRANSFORM_FLAG_BITS_MAX_ENUM_KHR}) ]

type SurfaceTransformFlags = SurfaceTransformFlagBits

-- VkCompositeAlphaFlagsKHR

enum "CompositeAlphaFlagBits" ''#{type VkCompositeAlphaFlagBitsKHR}
		[''Show, ''Eq, ''Bits, ''Storable] [
	-- VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
	-- VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
	-- VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
	-- VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
	-- FOO
	]

-- VkImageUsageFlags

-- VkSurfaceCapabilitiesKHR
