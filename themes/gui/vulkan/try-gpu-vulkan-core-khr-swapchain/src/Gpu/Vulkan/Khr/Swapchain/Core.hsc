{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Swapchain.Core (

	-- * ACQUIRE NEXT IMAGE

	acquireNextImage,

	-- * QUEUE PRESENT

	queuePresent,
	PresentInfo, pattern PresentInfo,
	presentInfoSType, presentInfoPNext,
	presentInfoWaitSemaphoreCount, presentInfoPWaitSemaphores,
	presentInfoSwapchainCount, presentInfoPSwapchains,
	presentInfoPImageIndices, presentInfoPResults,

	-- * CREATE AND DESTROY

	create, destroy, S, PtrS, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoSurface, createInfoMinImageCount,
	createInfoImageFormat, createInfoImageColorSpace, createInfoImageExtent,
	createInfoImageArrayLayers, createInfoImageUsage,
	createInfoImageSharingMode,
	createInfoQueueFamilyIndexCount, createInfoPQueueFamilyIndices,
	createInfoPreTransform, createInfoCompositeAlpha, createInfoPresentMode,
	createInfoClipped, createInfoOldSwapchain,

	-- * GET IMAGES

	getImages

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import Gpu.Vulkan.Core
import Gpu.Vulkan.TypeSynonyms.Core
import Gpu.Vulkan.AllocationCallbacks.Core qualified as AllocationCallbacks
import Gpu.Vulkan.Device.Core qualified as Device
import Gpu.Vulkan.Khr.Surface.Core qualified as Surface
import Gpu.Vulkan.Image.Core qualified as Image

import Gpu.Vulkan.Queue.Core qualified as Queue
import Gpu.Vulkan.Semaphore.Core qualified as Semaphore
import Gpu.Vulkan.Fence.Core qualified as Fence

#include <vulkan/vulkan.h>

spType :: #{type VkStructureType}
spType = #{const VK_STRUCTURE_TYPE_PRESENT_INFO_KHR}

data STag
type S = Ptr STag
type PtrS = Ptr S

foreign import ccall "vkAcquireNextImageKHR" acquireNextImage ::
	Device.D -> S -> #{type uint64_t} -> Semaphore.S -> Fence.F ->
	Ptr #{type uint32_t} -> IO #{type VkResult}

struct "PresentInfo" #{size VkPresentInfoKHR} #{alignment VkPresentInfoKHR} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPresentInfoKHR, sType} p spType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPresentInfoKHR, pNext} |],
		[| #{poke VkPresentInfoKHR, pNext} |]),
	("waitSemaphoreCount", ''#{type uint32_t},
		[| #{peek VkPresentInfoKHR, waitSemaphoreCount} |],
		[| #{poke VkPresentInfoKHR, waitSemaphoreCount} |]),
	("pWaitSemaphores", ''Semaphore.PtrS,
		[| #{peek VkPresentInfoKHR, pWaitSemaphores} |],
		[| #{poke VkPresentInfoKHR, pWaitSemaphores} |]),
	("swapchainCount", ''#{type uint32_t},
		[| #{peek VkPresentInfoKHR, swapchainCount} |],
		[| #{poke VkPresentInfoKHR, swapchainCount} |]),
	("pSwapchains", ''PtrS,
		[| #{peek VkPresentInfoKHR, pSwapchains} |],
		[| #{poke VkPresentInfoKHR, pSwapchains} |]),
	("pImageIndices", ''PtrUint32T,
		[| #{peek VkPresentInfoKHR, pImageIndices} |],
		[| #{poke VkPresentInfoKHR, pImageIndices} |]),
	("pResults", ''PtrResult,
		[| #{peek VkPresentInfoKHR, pResults} |],
		[| #{poke VkPresentInfoKHR, pResults} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkQueuePresentKHR" queuePresent ::
	Queue.Q -> Ptr PresentInfo -> IO #{type VkResult}

struct "CreateInfo" #{size VkSwapchainCreateInfoKHR}
		#{alignment VkSwapchainCreateInfoKHR} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSwapchainCreateInfoKHR, sType} p scType |]),
	("pNext", ''PtrVoid, [| #{peek VkSwapchainCreateInfoKHR, pNext} |],
		[| #{poke VkSwapchainCreateInfoKHR, pNext} |]),
	("flags", ''#{type VkSwapchainCreateFlagsKHR},
		[| #{peek VkSwapchainCreateInfoKHR, flags} |],
		[| #{poke VkSwapchainCreateInfoKHR, flags} |]),
	("surface", ''Surface.S,
		[| #{peek VkSwapchainCreateInfoKHR, surface} |],
		[| #{poke VkSwapchainCreateInfoKHR, surface} |]),
	("minImageCount", ''#{type uint32_t},
		[| #{peek VkSwapchainCreateInfoKHR, minImageCount} |],
		[| #{poke VkSwapchainCreateInfoKHR, minImageCount} |]),
	("imageFormat", ''#{type VkFormat},
		[| #{peek VkSwapchainCreateInfoKHR, imageFormat} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageFormat} |]),
	("imageColorSpace", ''#{type VkColorSpaceKHR},
		[| #{peek VkSwapchainCreateInfoKHR, imageColorSpace} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageColorSpace} |]),
	("imageExtent", ''Extent2d,
		[| #{peek VkSwapchainCreateInfoKHR, imageExtent} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageExtent} |]),
	("imageArrayLayers", ''#{type uint32_t},
		[| #{peek VkSwapchainCreateInfoKHR, imageArrayLayers} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageArrayLayers} |]),
	("imageUsage", ''#{type VkImageUsageFlags},
		[| #{peek VkSwapchainCreateInfoKHR, imageUsage} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageUsage} |]),
	("imageSharingMode", ''#{type VkSharingMode},
		[| #{peek VkSwapchainCreateInfoKHR, imageSharingMode} |],
		[| #{poke VkSwapchainCreateInfoKHR, imageSharingMode} |]),
	("queueFamilyIndexCount", ''#{type uint32_t},
		[| #{peek VkSwapchainCreateInfoKHR, queueFamilyIndexCount} |],
		[| #{poke VkSwapchainCreateInfoKHR, queueFamilyIndexCount} |]),
	("pQueueFamilyIndices", ''PtrUint32T,
		[| #{peek VkSwapchainCreateInfoKHR, pQueueFamilyIndices} |],
		[| #{poke VkSwapchainCreateInfoKHR, pQueueFamilyIndices} |]),
	("preTransform", ''#{type VkSurfaceTransformFlagBitsKHR},
		[| #{peek VkSwapchainCreateInfoKHR, preTransform} |],
		[| #{poke VkSwapchainCreateInfoKHR, preTransform} |]),
	("compositeAlpha", ''#{type VkCompositeAlphaFlagBitsKHR},
		[| #{peek VkSwapchainCreateInfoKHR, compositeAlpha} |],
		[| #{poke VkSwapchainCreateInfoKHR, compositeAlpha} |]),
	("presentMode", ''#{type VkPresentModeKHR},
		[| #{peek VkSwapchainCreateInfoKHR, presentMode} |],
		[| #{poke VkSwapchainCreateInfoKHR, presentMode} |]),
	("clipped", ''#{type VkBool32},
		[| #{peek VkSwapchainCreateInfoKHR, clipped} |],
		[| #{poke VkSwapchainCreateInfoKHR, clipped} |]),
	("oldSwapchain", ''S,
		[| #{peek VkSwapchainCreateInfoKHR, oldSwapchain} |],
		[| #{poke VkSwapchainCreateInfoKHR, oldSwapchain} |]) ]
	[''Show, ''Storable]

scType :: #{type VkStructureType}
scType = #{const VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR}

foreign import ccall "vkCreateSwapchainKHR" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr S ->
	IO #{type VkResult}

foreign import ccall "vkDestroySwapchainKHR" destroy ::
	Device.D -> S -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "vkGetSwapchainImagesKHR" getImages ::
	Device.D -> S -> Ptr #{type uint32_t} -> Ptr Image.I ->
	IO #{type VkResult}
