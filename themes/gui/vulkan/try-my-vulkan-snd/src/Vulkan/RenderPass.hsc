{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan
import Vulkan.Base
import Vulkan.Device (Device)

import qualified Vulkan.Attachment as Attachment
import qualified Vulkan.Subpass as Subpass

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

data RenderPassTag
type RenderPass = Ptr RenderPassTag

foreign import ccall "vkCreateRenderPass" create ::
	Device -> Ptr CreateInfo -> Ptr () -> Ptr RenderPass ->
	IO #{type VkResult}

foreign import ccall "vkDestroyRenderPass" destroy ::
	Device -> RenderPass -> Ptr () -> IO ()

struct "BeginInfo" #{size VkRenderPassBeginInfo}
		#{alignment VkRenderPassBeginInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkRenderPassBeginInfo, sType} p sTypeB |]),
	("pNext", ''PtrVoid,
		[| #{peek VkRenderPassBeginInfo, pNext} |],
		[| #{poke VkRenderPassBeginInfo, pNext} |]),
	("renderPass", ''RenderPass,
		[| #{peek VkRenderPassBeginInfo, renderPass } |],
		[| #{poke VkRenderPassBeginInfo, renderPass } |]),
	("framebuffer", ''Framebuffer,
		[| #{peek VkRenderPassBeginInfo, framebuffer} |],
		[| #{poke VkRenderPassBeginInfo, framebuffer} |]),
	("renderArea", ''Rect2d,
		[| #{peek VkRenderPassBeginInfo, renderArea} |],
		[| #{poke VkRenderPassBeginInfo, renderArea} |]),
	("clearValueCount", ''#{type uint32_t},
		[| #{peek VkRenderPassBeginInfo, clearValueCount} |],
		[| #{poke VkRenderPassBeginInfo, clearValueCount} |]),
	("pClearColorValueFloats", ''PtrFloat,
		[| #{peek VkRenderPassBeginInfo, pClearValues} |],
		[| #{poke VkRenderPassBeginInfo, pClearValues} |]) ]
	[''Show, ''Storable]
