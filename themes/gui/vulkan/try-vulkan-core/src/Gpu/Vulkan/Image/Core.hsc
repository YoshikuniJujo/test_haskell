{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image.Core (

	-- * CREATE AND DESTROY
	create, destroy, I, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoImageType, createInfoFormat, createInfoExtent,
	createInfoMipLevels, createInfoArrayLayers, createInfoSamples,
	createInfoTiling, createInfoUsage, createInfoSharingMode,
	createInfoQueueFamilyIndexCount, createInfoPQueueFamilyIndices,
	createInfoInitialLayout,

	-- * MEMORY: REQUIREMENTS AND BINDING

	getMemoryRequirements, bindMemory,

	-- * MEMORY BARRIER

	MemoryBarrier, pattern MemoryBarrier,
	memoryBarrierSType, memoryBarrierPNext,
	memoryBarrierSrcAccessMask, memoryBarrierDstAccessMask,
	memoryBarrierOldLayout, memoryBarrierNewLayout,
	memoryBarrierSrcQueueFamilyIndex, memoryBarrierDstQueueFamilyIndex,
	memoryBarrierImage, memoryBarrierSubresourceRange,

	-- ** SubresourceRange

	SubresourceRange, pattern SubresourceRange,
	subresourceRangeAspectMask, subresourceRangeBaseMipLevel,
	subresourceRangeLevelCount, subresourceRangeBaseArrayLayer,
	subresourceRangeLayerCount,

	-- * BLIT
	
	Blit, pattern Blit,
	blitSrcSubresource, blitSrcOffsets, blitDstSubresource, blitDstOffsets,

	-- ** SubresourceLayers

	SubresourceLayers, pattern SubresourceLayers,
	subresourceLayersAspectMask, subresourceLayersMipLevel,
	subresourceLayersBaseArrayLayer, subresourceLayersLayerCount,

	) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import Gpu.Vulkan.Core
import Gpu.Vulkan.TypeSynonyms.Core
import Gpu.Vulkan.AllocationCallbacks.Core qualified as AllocationCallbacks
import Gpu.Vulkan.Device.Core qualified as Device
import Gpu.Vulkan.Memory.Core qualified as Memory

#include <vulkan/vulkan.h>

struct "SubresourceRange" #{size VkImageSubresourceRange}
		#{alignment VkImageSubresourceRange} [
	("aspectMask", ''#{type VkImageAspectFlags},
		[| #{peek VkImageSubresourceRange, aspectMask} |],
		[| #{poke VkImageSubresourceRange, aspectMask} |]),
	("baseMipLevel", ''#{type uint32_t},
		[| #{peek VkImageSubresourceRange, baseMipLevel} |],
		[| #{poke VkImageSubresourceRange, baseMipLevel} |]),
	("levelCount", ''#{type uint32_t},
		[| #{peek VkImageSubresourceRange, levelCount} |],
		[| #{poke VkImageSubresourceRange, levelCount} |]),
	("baseArrayLayer", ''#{type uint32_t},
		[| #{peek VkImageSubresourceRange, baseArrayLayer} |],
		[| #{poke VkImageSubresourceRange, baseArrayLayer} |]),
	("layerCount", ''#{type uint32_t},
		[| #{peek VkImageSubresourceRange, layerCount} |],
		[| #{poke VkImageSubresourceRange, layerCount} |]) ]
	[''Show, ''Storable]

data ITag
type I = Ptr ITag

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO}

struct "CreateInfo" #{size VkImageCreateInfo} #{alignment VkImageCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkImageCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkImageCreateInfo, pNext} |],
		[| #{poke VkImageCreateInfo, pNext} |]),
	("flags", ''#{type VkImageCreateFlags},
		[| #{peek VkImageCreateInfo, flags} |],
		[| #{poke VkImageCreateInfo, flags} |]),
	("imageType", ''#{type VkImageType},
		[| #{peek VkImageCreateInfo, imageType} |],
		[| #{poke VkImageCreateInfo, imageType} |]),
	("format", ''#{type VkFormat},
		[| #{peek VkImageCreateInfo, format} |],
		[| #{poke VkImageCreateInfo, format} |]),
	("extent", ''Extent3d,
		[| #{peek VkImageCreateInfo, extent} |],
		[| #{poke VkImageCreateInfo, extent} |]),
	("mipLevels", ''#{type uint32_t},
		[| #{peek VkImageCreateInfo, mipLevels} |],
		[| #{poke VkImageCreateInfo, mipLevels} |]),
	("arrayLayers", ''#{type uint32_t},
		[| #{peek VkImageCreateInfo, arrayLayers} |],
		[| #{poke VkImageCreateInfo, arrayLayers} |]),
	("samples", ''#{type VkSampleCountFlagBits},
		[| #{peek VkImageCreateInfo, samples} |],
		[| #{poke VkImageCreateInfo, samples} |]),
	("tiling", ''#{type VkImageTiling},
		[| #{peek VkImageCreateInfo, tiling} |],
		[| #{poke VkImageCreateInfo, tiling} |]),
	("usage", ''#{type VkImageUsageFlags},
		[| #{peek VkImageCreateInfo, usage} |],
		[| #{poke VkImageCreateInfo, usage} |]),
	("sharingMode", ''#{type VkSharingMode},
		[| #{peek VkImageCreateInfo, sharingMode} |],
		[| #{poke VkImageCreateInfo, sharingMode} |]),
	("queueFamilyIndexCount", ''#{type uint32_t},
		[| #{peek VkImageCreateInfo, queueFamilyIndexCount} |],
		[| #{poke VkImageCreateInfo, queueFamilyIndexCount} |]),
	("pQueueFamilyIndices", ''PtrUint32T,
		[| #{peek VkImageCreateInfo, pQueueFamilyIndices} |],
		[| #{poke VkImageCreateInfo, pQueueFamilyIndices} |]),
	("initialLayout", ''#{type VkImageLayout},
		[| #{peek VkImageCreateInfo, initialLayout} |],
		[| #{poke VkImageCreateInfo, initialLayout} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateImage" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr I ->
	IO #{type VkResult}

foreign import ccall "vkGetImageMemoryRequirements" getMemoryRequirements ::
	Device.D -> I -> Ptr Memory.Requirements -> IO ()

foreign import ccall "vkBindImageMemory" bindMemory ::
	Device.D -> I -> Memory.M -> #{type VkDeviceSize} ->
	IO #{type VkResult}

mbType :: #{type VkStructureType}
mbType = #{const VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER}

struct "MemoryBarrier" #{size VkImageMemoryBarrier}
		#{alignment VkImageMemoryBarrier} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkImageMemoryBarrier, sType} p mbType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkImageMemoryBarrier, pNext} |],
		[| #{poke VkImageMemoryBarrier, pNext} |]),
	("srcAccessMask", ''#{type VkAccessFlags},
		[| #{peek VkImageMemoryBarrier, srcAccessMask} |],
		[| #{poke VkImageMemoryBarrier, srcAccessMask} |]),
	("dstAccessMask", ''#{type VkAccessFlags},
		[| #{peek VkImageMemoryBarrier, dstAccessMask} |],
		[| #{poke VkImageMemoryBarrier, dstAccessMask} |]),
	("oldLayout", ''#{type VkImageLayout},
		[| #{peek VkImageMemoryBarrier, oldLayout} |],
		[| #{poke VkImageMemoryBarrier, oldLayout} |]),
	("newLayout", ''#{type VkImageLayout},
		[| #{peek VkImageMemoryBarrier, newLayout} |],
		[| #{poke VkImageMemoryBarrier, newLayout} |]),
	("srcQueueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkImageMemoryBarrier, srcQueueFamilyIndex} |],
		[| #{poke VkImageMemoryBarrier, srcQueueFamilyIndex} |]),
	("dstQueueFamilyIndex", ''#{type uint32_t},
		[| #{peek VkImageMemoryBarrier, dstQueueFamilyIndex} |],
		[| #{poke VkImageMemoryBarrier, dstQueueFamilyIndex} |]),
	("image", ''I,
		[| #{peek VkImageMemoryBarrier, image} |],
		[| #{poke VkImageMemoryBarrier, image} |]),
	("subresourceRange", ''SubresourceRange,
		[| #{peek VkImageMemoryBarrier, subresourceRange} |],
		[| #{poke VkImageMemoryBarrier, subresourceRange} |]) ]
	[''Show, ''Storable]

struct "SubresourceLayers" #{size VkImageSubresourceLayers}
		#{alignment VkImageSubresourceLayers} [
	("aspectMask", ''#{type VkImageAspectFlags},
		[| #{peek VkImageSubresourceLayers, aspectMask} |],
		[| #{poke VkImageSubresourceLayers, aspectMask} |]),
	("mipLevel", ''#{type uint32_t},
		[| #{peek VkImageSubresourceLayers, mipLevel} |],
		[| #{poke VkImageSubresourceLayers, mipLevel} |]),
	("baseArrayLayer", ''#{type uint32_t},
		[| #{peek VkImageSubresourceLayers, baseArrayLayer} |],
		[| #{poke VkImageSubresourceLayers, baseArrayLayer} |]),
	("layerCount", ''#{type uint32_t},
		[| #{peek VkImageSubresourceLayers, layerCount} |],
		[| #{poke VkImageSubresourceLayers, layerCount} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkDestroyImage" destroy ::
	Device.D -> I -> Ptr AllocationCallbacks.A -> IO ()

struct "Blit" #{size VkImageBlit} #{alignment VkImageBlit} [
	("srcSubresource", ''SubresourceLayers,
		[| #{peek VkImageBlit, srcSubresource} |],
		[| #{poke VkImageBlit, srcSubresource} |]),
	("srcOffsets", ''ListOffset3d,
		[| \p -> peekArray 2 (#{ptr VkImageBlit, srcOffsets} p) |],
		[| \p os -> pokeArray
			(#{ptr VkImageBlit, srcOffsets} p) $ take 2 os |]),
	("dstSubresource", ''SubresourceLayers,
		[| #{peek VkImageBlit, dstSubresource} |],
		[| #{poke VkImageBlit, dstSubresource} |]),
	("dstOffsets", ''ListOffset3d,
		[| \p -> peekArray 2 (#{ptr VkImageBlit, dstOffsets} p) |],
		[| \p os -> pokeArray
			(#{ptr VkImageBlit, dstOffsets} p) $ take 2 os |]) ]
	[''Show, ''Storable]
