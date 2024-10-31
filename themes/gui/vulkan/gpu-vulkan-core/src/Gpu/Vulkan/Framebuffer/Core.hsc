{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Framebuffer.Core (

	-- * CREATE AND DESTROY

	create, destroy, F, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags, createInfoRenderPass,
	createInfoAttachmentCount, createInfoPAttachments,
	createInfoWidth, createInfoHeight, createInfoLayers

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device
import {-# SOURCE #-} qualified Gpu.Vulkan.RenderPass.Core as RenderPass
import qualified Gpu.Vulkan.ImageView.Core as ImageView

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO}

struct "CreateInfo" #{size VkFramebufferCreateInfo}
		#{alignment VkFramebufferCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkFramebufferCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkFramebufferCreateInfo, pNext} |],
		[| #{poke VkFramebufferCreateInfo, pNext} |]),
	("flags", ''#{type VkFramebufferCreateFlags},
		[| #{peek VkFramebufferCreateInfo, flags} |],
		[| #{poke VkFramebufferCreateInfo, flags} |]),
	("renderPass", ''RenderPass.R,
		[| #{peek VkFramebufferCreateInfo, renderPass} |],
		[| #{poke VkFramebufferCreateInfo, renderPass} |]),
	("attachmentCount", ''#{type uint32_t},
		[| #{peek VkFramebufferCreateInfo, attachmentCount} |],
		[| #{poke VkFramebufferCreateInfo, attachmentCount} |]),
	("pAttachments", ''ImageView.PtrI,
		[| #{peek VkFramebufferCreateInfo, pAttachments} |],
		[| #{poke VkFramebufferCreateInfo, pAttachments} |]),
	("width", ''#{type uint32_t},
		[| #{peek VkFramebufferCreateInfo, width} |],
		[| #{poke VkFramebufferCreateInfo, width} |]),
	("height", ''#{type uint32_t},
		[| #{peek VkFramebufferCreateInfo, height} |],
		[| #{poke VkFramebufferCreateInfo, height} |]),
	("layers", ''#{type uint32_t},
		[| #{peek VkFramebufferCreateInfo, layers} |],
		[| #{poke VkFramebufferCreateInfo, layers} |]) ]
	[''Show, ''Storable]

data FTag
type F = Ptr FTag

foreign import ccall "vkCreateFramebuffer" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr F ->
	IO #{type VkResult}

foreign import ccall "vkDestroyFramebuffer" destroy ::
	Device.D -> F -> Ptr AllocationCallbacks.A -> IO ()
