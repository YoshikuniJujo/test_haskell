{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Core (

	-- * CREATE AND DESTROY

	create, destroy, F, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,

	-- * WAIT AND RESET

	waitForFs, resetFs

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_FENCE_CREATE_INFO}

struct "CreateInfo" #{size VkFenceCreateInfo} #{alignment VkFenceCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkFenceCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkFenceCreateInfo, pNext} |],
		[| #{poke VkFenceCreateInfo, pNext} |]),
	("flags", ''#{type VkFenceCreateFlags},
		[| #{peek VkFenceCreateInfo, flags} |],
		[| #{poke VkFenceCreateInfo, flags} |]) ]
	[''Show, ''Storable]

data FTag
type F = Ptr FTag

foreign import ccall "vkCreateFence" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr F ->
	IO #{type VkResult}

foreign import ccall "vkDestroyFence" destroy ::
	Device.D -> F -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "vkWaitForFences" waitForFs ::
	Device.D -> #{type uint32_t} -> Ptr F -> #{type VkBool32} ->
	#{type uint64_t} -> IO #{type VkResult}

foreign import ccall "vkResetFences" resetFs ::
	Device.D -> #{type uint32_t} -> Ptr F -> IO #{type VkResult}
