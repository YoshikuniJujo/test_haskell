{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Core (

	-- * ALLOCATE AND FREE

	allocate, free, M, AllocateInfo, pattern AllocateInfo,
	allocateInfoSType, allocateInfoPNext, allocateInfoAllocationSize,
	allocateInfoMemoryTypeIndex,

	-- * MAP AND UNMAP

	map, unmap,

	-- * REQUIREMENTS

	Requirements, pattern Requirements,
	requirementsSize, requirementsAlignment, requirementsMemoryTypeBits,

	-- * TYPE

	MType, ListMType, pattern MType, mTypePropertyFlags, mTypeHeapIndex, maxTypes,

	-- * HEAP

	Heap, ListHeap, pattern Heap, heapSize, heapFlags, maxHeaps,

	-- * BARRIER

	Barrier, pattern Barrier,
	barrierSType, barrierPNext, barrierSrcAccessMask, barrierDstAccessMask

	) where

import Prelude hiding (map)

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks

import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Core as Device

#include <vulkan/vulkan.h>

data MTag
type M = Ptr MTag

struct "Requirements" #{size VkMemoryRequirements}
		#{alignment VkMemoryRequirements} [
	("size", ''#{type VkDeviceSize},
		[| #{peek VkMemoryRequirements, size} |],
		[| #{poke VkMemoryRequirements, size} |]),
	("alignment", ''#{type VkDeviceSize},
		[| #{peek VkMemoryRequirements, alignment} |],
		[| #{poke VkMemoryRequirements, alignment} |]),
	("memoryTypeBits", ''#{type uint32_t},
		[| #{peek VkMemoryRequirements, memoryTypeBits} |],
		[| #{poke VkMemoryRequirements, memoryTypeBits} |]) ]
	[''Show, ''Storable]

struct "MType" #{size VkMemoryType} #{alignment VkMemoryType} [
	("propertyFlags", ''#{type VkMemoryPropertyFlags},
		[| #{peek VkMemoryType, propertyFlags} |],
		[| #{poke VkMemoryType, propertyFlags} |]),
	("heapIndex", ''#{type uint32_t},
		[| #{peek VkMemoryType, heapIndex} |],
		[| #{poke VkMemoryType, heapIndex} |]) ]
	[''Show, ''Storable]

type ListMType = [MType]

struct "Heap" #{size VkMemoryHeap} #{alignment VkMemoryHeap} [
	("size", ''#{type VkDeviceSize},
		[| #{peek VkMemoryHeap, size} |],
		[| #{poke VkMemoryHeap, size} |]),
	("flags", ''#{type VkMemoryHeapFlags},
		[| #{peek VkMemoryHeap, flags} |],
		[| #{poke VkMemoryHeap, flags} |]) ]
	[''Show, ''Storable]

type ListHeap = [Heap]

maxTypes, maxHeaps :: Integral n => n
maxTypes = #{const VK_MAX_MEMORY_TYPES}
maxHeaps = #{const VK_MAX_MEMORY_HEAPS}

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO}

struct "AllocateInfo" #{size VkMemoryAllocateInfo}
		#{alignment VkMemoryAllocateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkMemoryAllocateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkMemoryAllocateInfo, pNext} |],
		[| #{poke VkMemoryAllocateInfo, pNext} |]),
	("allocationSize", ''#{type VkDeviceSize},
		[| #{peek VkMemoryAllocateInfo, allocationSize} |],
		[| #{poke VkMemoryAllocateInfo, allocationSize} |]),
	("memoryTypeIndex", ''#{type uint32_t},
		[| #{peek VkMemoryAllocateInfo, memoryTypeIndex} |],
		[| #{poke VkMemoryAllocateInfo, memoryTypeIndex} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkAllocateMemory" allocate ::
	Device.D -> Ptr AllocateInfo -> Ptr AllocationCallbacks.A ->
	Ptr M -> IO #{type VkResult}

foreign import ccall "vkFreeMemory" free ::
	Device.D -> M -> Ptr AllocationCallbacks.A -> IO ()

foreign import ccall "vkMapMemory" map ::
	Device.D -> M -> #{type VkDeviceSize} -> #{type VkDeviceSize} ->
	#{type VkMemoryMapFlags} -> Ptr (Ptr a) -> IO #{type VkResult}

foreign import ccall "vkUnmapMemory" unmap :: Device.D -> M -> IO ()

bType :: #{type VkStructureType}
bType = #{const VK_STRUCTURE_TYPE_MEMORY_BARRIER}

struct "Barrier" #{size VkMemoryBarrier} #{alignment VkMemoryBarrier} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkMemoryBarrier, sType} p bType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkMemoryBarrier, pNext} |],
		[| #{poke VkMemoryBarrier, pNext} |]),
	("srcAccessMask", ''#{type VkAccessFlags},
		[| #{peek VkMemoryBarrier, srcAccessMask} |],
		[| #{poke VkMemoryBarrier, srcAccessMask} |]),
	("dstAccessMask", ''#{type VkAccessFlags},
		[| #{peek VkMemoryBarrier, dstAccessMask} |],
		[| #{poke VkMemoryBarrier, dstAccessMask} |]) ]
	[''Show, ''Storable]
