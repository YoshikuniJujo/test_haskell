{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.Fence

#include <vulkan/vulkan.h>

newtype ApiVersion = ApiVersion #{type uint32_t}
	deriving (Show, Eq, Ord, Storable)

struct "ApplicationInfo" #{size VkApplicationInfo}
		#{alignment VkApplicationInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkApplicationInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_APPLICATION_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkApplicationInfo, pNext} |],
		[| #{poke VkApplicationInfo, pNext} |]),
	("pApplicationName", ''CString,
		[| #{peek VkApplicationInfo, pApplicationName} |],
		[| #{poke VkApplicationInfo, pApplicationName} |]),
	("applicationVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, applicationVersion} |],
		[| #{poke VkApplicationInfo, applicationVersion} |]),
	("pEngineName", ''CString,
		[| #{peek VkApplicationInfo, pEngineName} |],
		[| #{poke VkApplicationInfo, pEngineName} |]),
	("engineVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, engineVersion} |],
		[| #{poke VkApplicationInfo, engineVersion} |]),
	("apiVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, apiVersion} |],
		[| #{poke VkApplicationInfo, apiVersion} |])
	]
	[''Show]

foreign import capi "vulkan/vulkan.h VK_MAKE_API_VERSION" makeApiVersion ::
	Word8 -> Word8 -> Word8 -> Word16 -> ApiVersion

type PtrApplicationInfo = Ptr ApplicationInfo

struct "Extent2d" #{size VkExtent2D} #{alignment VkExtent2D} [
	("width", ''#{type uint32_t}, [| #{peek VkExtent2D, width} |],
		[| #{poke VkExtent2D, width} |]),
	("height", ''#{type uint32_t}, [| #{peek VkExtent2D, height} |],
		[| #{poke VkExtent2D, height} |]) ]
	[''Show, ''Storable]

struct "Extent3d" #{size VkExtent3D} #{alignment VkExtent3D} [
	("width", ''#{type uint32_t}, [| #{peek VkExtent3D, width} |],
		[| #{poke VkExtent3D, width} |]),
	("height", ''#{type uint32_t}, [| #{peek VkExtent3D, height} |],
		[| #{poke VkExtent3D, height} |]),
	("depth", ''#{type uint32_t}, [| #{peek VkExtent3D, depth} |],
		[| #{poke VkExtent3D, depth} |]) ]
	[''Show, ''Storable]

imageUsageColorAttachmentBit :: #{type VkImageUsageFlagBits}
imageUsageColorAttachmentBit = #{const VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT}

sharingModeExclusive :: #{type VkSharingMode}
sharingModeExclusive = #{const VK_SHARING_MODE_EXCLUSIVE}

type Format = #{type VkFormat}

struct "Viewport" #{size VkViewport} #{alignment VkViewport} [
	("x", ''#{type float}, [| #{peek VkViewport, x} |],
		[| #{poke VkViewport, x} |]),
	("y", ''#{type float}, [| #{peek VkViewport, y} |],
		[| #{poke VkViewport, y} |]),
	("width", ''#{type float}, [| #{peek VkViewport, width} |],
		[| #{poke VkViewport, width} |]),
	("height", ''#{type float}, [| #{peek VkViewport, height} |],
		[| #{poke VkViewport, height} |]),
	("minDepth", ''#{type float}, [| #{peek VkViewport, minDepth} |],
		[| #{poke VkViewport, minDepth} |]),
	("maxDepth", ''#{type float}, [| #{peek VkViewport, maxDepth} |],
		[| #{poke VkViewport, maxDepth} |]) ]
	[''Show, ''Storable]

type PtrViewport = Ptr Viewport

struct "Offset2d" #{size VkOffset2D} #{alignment VkOffset2D} [
	("x", ''#{type int32_t}, [| #{peek VkOffset2D, x} |],
		[| #{poke VkOffset2D, x} |]),
	("y", ''#{type int32_t}, [| #{peek VkOffset2D, y} |],
		[| #{poke VkOffset2D, y} |]) ]
	[''Show, ''Storable]

struct "Rect2d" #{size VkRect2D} #{alignment VkRect2D} [
	("offset", ''Offset2d, [| #{peek VkRect2D, offset} |],
		[| #{poke VkRect2D, offset} |]),
	("extent", ''Extent2d, [| #{peek VkRect2D, extent} |],
		[| #{poke VkRect2D, extent} |]) ]
	[''Show, ''Storable]

type PtrRect2d = Ptr Rect2d

data SemaphoreTag
type Semaphore = Ptr SemaphoreTag
type PtrSemaphore = Ptr Semaphore

type PtrPipelineStageFlags = Ptr #{type VkPipelineStageFlags}

data CommandBufferTag
type CommandBuffer = Ptr CommandBufferTag
type PtrCommandBuffer = Ptr CommandBuffer

sTypeS :: #{type VkStructureType}
sTypeS = #{const VK_STRUCTURE_TYPE_SUBMIT_INFO}

struct "SubmitInfo" #{size VkSubmitInfo} #{alignment VkSubmitInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSubmitInfo, sType} p sTypeS |]),
	("pNext", ''PtrVoid,
		[| #{peek VkSubmitInfo, pNext} |],
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
	("signalSemaphoreCount", ''#{type int32_t},
		[| #{peek VkSubmitInfo, signalSemaphoreCount} |],
		[| #{poke VkSubmitInfo, signalSemaphoreCount} |]),
	("pSignalSemaphores", ''PtrSemaphore,
		[| #{peek VkSubmitInfo, pSignalSemaphores} |],
		[| #{poke VkSubmitInfo, pSignalSemaphores} |]) ]
	[''Show, ''Storable]

data QueueTag
type Queue = Ptr QueueTag

foreign import ccall "vkQueueSubmit" queueSubmit ::
	Queue -> #{type uint32_t} -> Ptr SubmitInfo -> Fence ->
	IO #{type VkResult}

foreign import ccall "vkQueueWaitIdle" queueWaitIdle ::
	Queue -> IO #{type VkResult}
