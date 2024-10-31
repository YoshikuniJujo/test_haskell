{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Core (

	-- * CREATE AND DESTROY

	create, destroy, R, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoAttachmentCount, createInfoPAttachments,
	createInfoSubpassCount, createInfoPSubpasses,
	createInfoDependencyCount, createInfoPDependencies,

	-- * BEGIN INFO

	BeginInfo, pattern BeginInfo,
	beginInfoSType, beginInfoPNext,
	beginInfoRenderPass, beginInfoFramebuffer, beginInfoRenderArea,
	beginInfoClearValueCount, beginInfoPClearValues

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import Gpu.Vulkan.Core

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.Attachment.Core as Attachment
import qualified Gpu.Vulkan.Subpass.Core as Subpass
import qualified Gpu.Vulkan.Framebuffer.Core as Framebuffer

#include <vulkan/vulkan.h>

sTypeC, sTypeB :: #{type VkStructureType}
sTypeC = #{const VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO}
sTypeB = #{const VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO}

struct "CreateInfo" #{size VkRenderPassCreateInfo}
		#{alignment VkRenderPassCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkRenderPassCreateInfo, sType} p sTypeC |]),
	("pNext", ''PtrVoid,
		[| #{peek VkRenderPassCreateInfo, pNext} |],
		[| #{poke VkRenderPassCreateInfo, pNext} |]),
	("flags", ''#{type VkRenderPassCreateFlags},
		[| #{peek VkRenderPassCreateInfo, flags} |],
		[| #{poke VkRenderPassCreateInfo, flags} |]),
	("attachmentCount", ''#{type uint32_t},
		[| #{peek VkRenderPassCreateInfo, attachmentCount} |],
		[| #{poke VkRenderPassCreateInfo, attachmentCount} |]),
	("pAttachments", ''Attachment.PtrDescription,
		[| #{peek VkRenderPassCreateInfo, pAttachments} |],
		[| #{poke VkRenderPassCreateInfo, pAttachments} |]),
	("subpassCount", ''#{type uint32_t},
		[| #{peek VkRenderPassCreateInfo, subpassCount} |],
		[| #{poke VkRenderPassCreateInfo, subpassCount} |]),
	("pSubpasses", ''Subpass.PtrDescription,
		[| #{peek VkRenderPassCreateInfo, pSubpasses} |],
		[| #{poke VkRenderPassCreateInfo, pSubpasses} |]),
	("dependencyCount", ''#{type uint32_t},
		[| #{peek VkRenderPassCreateInfo, dependencyCount} |],
		[| #{poke VkRenderPassCreateInfo, dependencyCount} |]),
	("pDependencies", ''Subpass.PtrDependency,
		[| #{peek VkRenderPassCreateInfo, pDependencies} |],
		[| #{poke VkRenderPassCreateInfo, pDependencies} |]) ]
	[''Show, ''Storable]

data RTag
type R = Ptr RTag

foreign import ccall "vkCreateRenderPass" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr R ->
	IO #{type VkResult}

foreign import ccall "vkDestroyRenderPass" destroy ::
	Device.D -> R -> Ptr AllocationCallbacks.A -> IO ()

struct "BeginInfo" #{size VkRenderPassBeginInfo}
		#{alignment VkRenderPassBeginInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkRenderPassBeginInfo, sType} p sTypeB |]),
	("pNext", ''PtrVoid,
		[| #{peek VkRenderPassBeginInfo, pNext} |],
		[| #{poke VkRenderPassBeginInfo, pNext} |]),
	("renderPass", ''R,
		[| #{peek VkRenderPassBeginInfo, renderPass } |],
		[| #{poke VkRenderPassBeginInfo, renderPass } |]),
	("framebuffer", ''Framebuffer.F,
		[| #{peek VkRenderPassBeginInfo, framebuffer} |],
		[| #{poke VkRenderPassBeginInfo, framebuffer} |]),
	("renderArea", ''Rect2d,
		[| #{peek VkRenderPassBeginInfo, renderArea} |],
		[| #{poke VkRenderPassBeginInfo, renderArea} |]),
	("clearValueCount", ''#{type uint32_t},
		[| #{peek VkRenderPassBeginInfo, clearValueCount} |],
		[| #{poke VkRenderPassBeginInfo, clearValueCount} |]),
	("pClearValues", ''PtrClearValue,
		[| #{peek VkRenderPassBeginInfo, pClearValues} |],
		[| #{poke VkRenderPassBeginInfo, pClearValues} |]) ]
	[''Show, ''Storable]
