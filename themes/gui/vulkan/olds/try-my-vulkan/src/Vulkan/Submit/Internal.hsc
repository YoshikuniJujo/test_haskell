{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Submit.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.Semaphore
import Vulkan.PipelineStageFlagBits
import Vulkan.CommandBuffer

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "Info" #{size VkSubmitInfo} #{alignment VkSubmitInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSubmitInfo, sType} p ST.submitInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkSubmitInfo, pNext} |],
		[| #{poke VkSubmitInfo, pNext} |]),
	("waitSemaphoreCount", ''#{type uint32_t},
		[| #{peek VkSubmitInfo, waitSemaphoreCount} |],
		[| #{poke VkSubmitInfo, waitSemaphoreCount} |]),
	("pWaitSemaphores", ''PtrSemaphore,
		[| #{peek VkSubmitInfo, pWaitSemaphores} |],
		[| #{poke VkSubmitInfo, pWaitSemaphores} |]),
	("pWaitDstStageMask", ''PtrPipelineStageFlags,
		[| #{peek VkSubmitInfo, pWaitDstStageMask} |],
		[| #{poke VkSubmitInfo, pWaitDstStageMask} |]),
	("commandBufferCount", ''#{type uint32_t},
		[| #{peek VkSubmitInfo, commandBufferCount} |],
		[| #{poke VkSubmitInfo, commandBufferCount} |]),
	("pCommandBuffers", ''PtrCommandBuffer,
		[| #{peek VkSubmitInfo, pCommandBuffers} |],
		[| #{poke VkSubmitInfo, pCommandBuffers} |]),
	("signalSemaphoreCount", ''#{type uint32_t},
		[| #{peek VkSubmitInfo, signalSemaphoreCount} |],
		[| #{poke VkSubmitInfo, signalSemaphoreCount} |]),
	("pSignalSemaphores", ''PtrSemaphore,
		[| #{peek VkSubmitInfo, pSignalSemaphores} |],
		[| #{poke VkSubmitInfo, pSignalSemaphores} |]) ]
	[''Show, ''Storable]
