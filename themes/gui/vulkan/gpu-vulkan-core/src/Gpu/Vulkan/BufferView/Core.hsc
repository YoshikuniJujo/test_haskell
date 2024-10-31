{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView.Core (

	-- * CREATE AND DESTROY

	create, destroy, B, PtrB,
	CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoBuffer, createInfoFormat, createInfoOffset, createInfoRange

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.Buffer.Core as Buffer

#include <vulkan/vulkan.h>

stype :: #{type VkStructureType}
stype = #{const VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO}

struct "CreateInfo" #{size VkBufferViewCreateInfo}
		#{alignment VkBufferViewCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkBufferViewCreateInfo, sType} p stype |]),
	("pNext", ''PtrVoid,
		[| #{peek VkBufferViewCreateInfo, pNext} |],
		[| #{poke VkBufferViewCreateInfo, pNext} |]),
	("flags", ''#{type VkBufferViewCreateFlags},
		[| #{peek VkBufferViewCreateInfo, flags} |],
		[| #{poke VkBufferViewCreateInfo, flags} |]),
	("buffer", ''Buffer.B,
		[| #{peek VkBufferViewCreateInfo, buffer} |],
		[| #{poke VkBufferViewCreateInfo, buffer} |]),
	("format", ''#{type VkFormat},
		[| #{peek VkBufferViewCreateInfo, format} |],
		[| #{poke VkBufferViewCreateInfo, format} |]),
	("offset", ''#{type VkDeviceSize},
		[| #{peek VkBufferViewCreateInfo, offset} |],
		[| #{poke VkBufferViewCreateInfo, offset} |]),
	("range", ''#{type VkDeviceSize},
		[| #{peek VkBufferViewCreateInfo, range} |],
		[| #{poke VkBufferViewCreateInfo, range} |]) ]
	[''Show, ''Storable]

data BTag
type B = Ptr BTag

type PtrB = Ptr B

foreign import ccall "vkCreateBufferView" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> PtrB ->
	IO #{type VkResult}

foreign import ccall "vkDestroyBufferView" destroy ::
	Device.D -> B -> Ptr AllocationCallbacks.A -> IO ()
