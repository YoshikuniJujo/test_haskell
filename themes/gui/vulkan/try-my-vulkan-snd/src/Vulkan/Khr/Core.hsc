{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base

import qualified Vulkan.Device.Core as Device
import qualified Vulkan.Semaphore.Core as Semaphore
import qualified Vulkan.Fence.Core as Fence
import qualified Vulkan.Queue.Core as Queue
import qualified Vulkan.Khr.Swapchain.Core as Swapchain

#include <vulkan/vulkan.h>

compositeAlphaOpaqueBit :: #{type VkCompositeAlphaFlagBitsKHR}
compositeAlphaOpaqueBit = #{const VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR}

foreign import ccall "vkAcquireNextImageKHR" acquireNextImage ::
	Device.D -> Swapchain.S -> #{type uint64_t} -> Semaphore.S -> Fence.F ->
	Ptr #{type uint32_t} -> IO #{type VkResult}

sTypeP :: #{type VkStructureType}
sTypeP = #{const VK_STRUCTURE_TYPE_PRESENT_INFO_KHR}

struct "PresentInfo" #{size VkPresentInfoKHR} #{alignment VkPresentInfoKHR} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPresentInfoKHR, sType} p sTypeP |]),
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
	("pSwapchains", ''Swapchain.PtrS,
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
