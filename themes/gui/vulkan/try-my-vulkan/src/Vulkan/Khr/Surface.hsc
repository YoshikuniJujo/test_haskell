{-# LANGUAGE TemplateHaskell, TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Surface where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Control.Monad.Cont
import Data.Bits
import Data.Word

import Vulkan.Base
import Vulkan.Format
import Vulkan.Exception
import Vulkan.Instance
import Vulkan.AllocationCallbacks
import Vulkan.PhysicalDevice
import Vulkan.Image

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

enum "CompositeAlphaFlagBits" ''#{type VkCompositeAlphaFlagBitsKHR}
		[''Show, ''Eq, ''Bits, ''Storable] [
	("CompositeAlphaOpaqueBit", #{const VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR}),
	("CompositeAlphaPreMultipleBit",
		#{const VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR}),
	("CompositeAlphaPostMultipliedBit",
		#{const VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR}),
	("CompositeAlphaInheritBit",
		#{const VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR}),
	("CompositeAlphaFlagBitsMaxEnum",
		#{const VK_COMPOSITE_ALPHA_FLAG_BITS_MAX_ENUM_KHR}) ]

type CompositeAlphaFlags = CompositeAlphaFlagBits

struct "SurfaceCapabilities" #{size VkSurfaceCapabilitiesKHR}
		#{alignment VkSurfaceCapabilitiesKHR} [
	("minImageCount", ''#{type uint32_t},
		[| #{peek VkSurfaceCapabilitiesKHR, minImageCount} |],
		[| #{poke VkSurfaceCapabilitiesKHR, minImageCount} |]),
	("maxImageCount", ''#{type uint32_t},
		[| #{peek VkSurfaceCapabilitiesKHR, maxImageCount} |],
		[| #{poke VkSurfaceCapabilitiesKHR, maxImageCount} |]),
	("currentExtent", ''Extent2d,
		[| #{peek VkSurfaceCapabilitiesKHR, currentExtent} |],
		[| #{poke VkSurfaceCapabilitiesKHR, currentExtent} |]),
	("minImageExtent", ''Extent2d,
		[| #{peek VkSurfaceCapabilitiesKHR, minImageExtent} |],
		[| #{poke VkSurfaceCapabilitiesKHR, minImageExtent} |]),
	("maxImageExtent", ''Extent2d,
		[| #{peek VkSurfaceCapabilitiesKHR, maxImageExtent} |],
		[| #{poke VkSurfaceCapabilitiesKHR, maxImageExtent} |]),
	("maxImageArrayLayers", ''#{type uint32_t},
		[| #{peek VkSurfaceCapabilitiesKHR, maxImageArrayLayers} |],
		[| #{poke VkSurfaceCapabilitiesKHR, maxImageArrayLayers} |]),
	("supportedTransforms", ''SurfaceTransformFlags,
		[| #{peek VkSurfaceCapabilitiesKHR, supportedTransforms} |],
		[| #{poke VkSurfaceCapabilitiesKHR, supportedTransforms} |]),
	("currentTransform", ''SurfaceTransformFlagBits,
		[| #{peek VkSurfaceCapabilitiesKHR, currentTransform} |],
		[| #{poke VkSurfaceCapabilitiesKHR, currentTransform} |]),
	("supportedCompositeAlpha", ''CompositeAlphaFlags,
		[| #{peek VkSurfaceCapabilitiesKHR, supportedCompositeAlpha} |],
		[| #{poke VkSurfaceCapabilitiesKHR,
			supportedCompositeAlpha} |]),
	("supportedUsageFlags", ''ImageUsageFlags,
		[| #{peek VkSurfaceCapabilitiesKHR, supportedUsageFlags} |],
		[| #{poke VkSurfaceCapabilitiesKHR, supportedUsageFlags} |]) ]
	[''Show, ''Storable]

getPhysicalDeviceSurfaceCapabilities ::
	PhysicalDevice -> Surface -> IO SurfaceCapabilities
getPhysicalDeviceSurfaceCapabilities phdv sfc = ($ pure) $ runContT do
	psc <- ContT alloca
	lift do	r <- c_vkGetPhysicalDeviceSurfaceCapabilitiesKHR phdv sfc psc
		throwUnlessSuccess r
		peek psc

foreign import ccall "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
	c_vkGetPhysicalDeviceSurfaceCapabilitiesKHR ::
	PhysicalDevice -> Surface -> Ptr SurfaceCapabilities -> IO Result

enum "ColorSpace" ''#{type VkColorSpaceKHR} [''Show, ''Eq, ''Storable] [
	("ColorSpaceSrgbNonlinear", #{const VK_COLOR_SPACE_SRGB_NONLINEAR_KHR})
	-- VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
	-- ...
	-- VK_COLOR_SPACE_DCI_P3_LINEAR_EXT
	]

struct "SurfaceFormat" #{size VkSurfaceFormatKHR}
		#{alignment VkSurfaceFormatKHR} [
	("format", ''Format, [| #{peek VkSurfaceFormatKHR, format} |],
		[| #{poke VkSurfaceFormatKHR, format} |]),
	("colorSpace", ''ColorSpace,
		[| #{peek VkSurfaceFormatKHR, colorSpace} |],
		[| #{poke VkSurfaceFormatKHR, colorSpace} |]) ]
	[''Show, ''Eq, ''Storable]

getPhysicalDeviceSurfaceFormats ::
	PhysicalDevice -> Surface -> IO [SurfaceFormat]
getPhysicalDeviceSurfaceFormats phdv sfc = ($ pure) $ runContT do
	pn <- ContT alloca
	n <- lift do
		r <- c_vkGetPhysicalDeviceSurfaceFormatsKHR phdv sfc pn NullPtr
		throwUnlessSuccess r
		fromIntegral <$> peek pn
	if n == 0 then pure [] else do
		psf <- ContT $ allocaArray n
		lift do	r <- c_vkGetPhysicalDeviceSurfaceFormatsKHR phdv sfc pn psf
			throwUnlessSuccess r
			peekArray n psf

foreign import ccall "vkGetPhysicalDeviceSurfaceFormatsKHR"
	c_vkGetPhysicalDeviceSurfaceFormatsKHR ::
	PhysicalDevice -> Surface -> Ptr #{type uint32_t} ->
	Ptr SurfaceFormat -> IO Result

enum "PresentMode" ''#{type VkPresentModeKHR} [''Show, ''Eq, ''Storable] [
	("PresentModeImmediate", #{const VK_PRESENT_MODE_IMMEDIATE_KHR}),
	("PresentModeMailbox", #{const VK_PRESENT_MODE_MAILBOX_KHR}),
	("PresentModeFifo", #{const VK_PRESENT_MODE_FIFO_KHR}),
	("PresentModeFifoRelaxed", #{const VK_PRESENT_MODE_FIFO_RELAXED_KHR}),
	("PresentModeSharedDemandRefresh",
		#{const VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR}),
	("PresentModeSharedContinuousRefresh",
		#{const VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR})
	]

getPhysicalDeviceSurfacePresentModes ::
	PhysicalDevice -> Surface -> IO [PresentMode]
getPhysicalDeviceSurfacePresentModes phdv sfc = ($ pure) $ runContT do
	pn <- ContT alloca
	n <- lift do
		r <- c_vkGetPhysicalDeviceSurfacePresentModesKHR
			phdv sfc pn NullPtr
		throwUnlessSuccess r
		fromIntegral <$> peek pn
	if n == 0 then pure [] else do
		ppm <- ContT $ allocaArray n
		lift do	r <- c_vkGetPhysicalDeviceSurfacePresentModesKHR phdv sfc pn ppm
			throwUnlessSuccess r
			peekArray n ppm

foreign import ccall "vkGetPhysicalDeviceSurfacePresentModesKHR"
	c_vkGetPhysicalDeviceSurfacePresentModesKHR ::
	PhysicalDevice -> Surface -> Ptr #{type uint32_t} ->
	Ptr PresentMode -> IO Result
