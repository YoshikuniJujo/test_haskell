{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Semaphore.Core (

	-- * CREATE AND DESTROY

	create, destroy, S, PtrS, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,

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
sType = #{const VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO}

struct "CreateInfo" #{size VkSemaphoreCreateInfo}
		#{alignment VkSemaphoreCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSemaphoreCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkSemaphoreCreateInfo, pNext} |],
		[| #{poke VkSemaphoreCreateInfo, pNext} |]),
	("flags", ''#{type VkSemaphoreCreateFlags},
		[| #{peek VkSemaphoreCreateInfo, flags} |],
		[| #{poke VkSemaphoreCreateInfo, flags} |]) ]
	[''Show, ''Storable]

data STag
type S = Ptr STag
type PtrS = Ptr S

foreign import ccall "vkCreateSemaphore" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A ->
	Ptr S -> IO #{type VkResult}

foreign import ccall "vkDestroySemaphore" destroy ::
	Device.D -> S -> Ptr AllocationCallbacks.A -> IO ()
