{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.Semaphore.Core as Semaphore
import qualified Gpu.Vulkan.Fence.Core as Fence
import qualified Gpu.Vulkan.Queue.Core as Queue
import qualified Gpu.Vulkan.Khr.Swapchain.Core as Swapchain

#include <vulkan/vulkan.h>

type PtrResult = Ptr #{type VkResult}
type PtrUint32T = Ptr #{type uint32_t}

foreign import ccall "vkAcquireNextImageKHR" acquireNextImage ::
	Device.D -> Swapchain.S -> #{type uint64_t} -> Semaphore.S -> Fence.F ->
	Ptr #{type uint32_t} -> IO #{type VkResult}

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_PRESENT_INFO_KHR}

struct "PresentInfo" #{size VkPresentInfoKHR} #{alignment VkPresentInfoKHR} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPresentInfoKHR, sType} p sType |]),
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
