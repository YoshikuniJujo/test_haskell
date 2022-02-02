{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Present.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.Semaphore
import Vulkan.Khr.Swapchain.Internal as I

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "Info" #{size VkPresentInfoKHR} #{alignment VkPresentInfoKHR} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPresentInfoKHR, sType}
			p ST.presentInfoKhr |]),
	("pNext", ''PtrVoid, [| #{peek VkPresentInfoKHR, pNext} |],
		[| #{poke VkPresentInfoKHR, pNext} |]),
	("waitSemaphoreCount", ''#{type uint32_t},
		[| #{peek VkPresentInfoKHR, waitSemaphoreCount} |],
		[| #{poke VkPresentInfoKHR, waitSemaphoreCount} |]),
	("pWaitSemaphores", ''PtrSemaphore,
		[| #{peek VkPresentInfoKHR, pWaitSemaphores} |],
		[| #{poke VkPresentInfoKHR, pWaitSemaphores} |]),
	("swapchainCount", ''#{type uint32_t},
		[| #{peek VkPresentInfoKHR, swapchainCount} |],
		[| #{poke VkPresentInfoKHR, swapchainCount} |]),
	("pSwapchains", ''I.PtrSwapchain,
		[| #{peek VkPresentInfoKHR, pSwapchains} |],
		[| #{poke VkPresentInfoKHR, pSwapchains} |]),
	("pImageIndices", ''PtrUint32T,
		[| #{peek VkPresentInfoKHR, pImageIndices} |],
		[| #{poke VkPresentInfoKHR, pImageIndices} |]),
	("pResults", ''PtrResult,
		[| #{peek VkPresentInfoKHR, pResults} |],
		[| #{poke VkPresentInfoKHR, pResults} |]) ]
	[''Show, ''Storable]
