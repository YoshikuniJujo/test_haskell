{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandPool.Core (

	-- * CREATE, DESTROY AND RESET

	create, destroy, reset, C,
	CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext,
	createInfoFlags, createInfoQueueFamilyIndex

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
sType = #{const VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO}

struct "CreateInfo" #{size VkCommandPoolCreateInfo}
		#{alignment VkCommandPoolCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCommandPoolCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkCommandPoolCreateInfo, pNext} |],
		[| #{poke VkCommandPoolCreateInfo, pNext} |]),
	("flags", ''#{type VkCommandPoolCreateFlags},
		[| #{peek VkCommandPoolCreateInfo, flags} |],
		[| #{poke VkCommandPoolCreateInfo, flags} |]),
	("queueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkCommandPoolCreateInfo, queueFamilyIndex} |],
		[| #{poke VkCommandPoolCreateInfo, queueFamilyIndex} |]) ]
	[''Show, ''Storable]

data CTag
type C = Ptr CTag

foreign import ccall "vkCreateCommandPool" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr C ->
	IO #{type VkResult}

foreign import ccall "vkDestroyCommandPool" destroy ::
	Device.D -> C -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "vkResetCommandPool" reset ::
	Device.D -> C -> #{type VkCommandPoolResetFlags} -> IO ()
