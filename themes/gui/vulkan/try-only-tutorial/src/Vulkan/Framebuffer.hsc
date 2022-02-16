{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Framebuffer where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base
import Vulkan.Device (Device)

import qualified Vulkan.RenderPass as RenderPass
import qualified Vulkan.ImageView as ImageView

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
	("renderPass", ''RenderPass.RenderPass,
		[| #{peek VkFramebufferCreateInfo, renderPass} |],
		[| #{poke VkFramebufferCreateInfo, renderPass} |]),
	("attachmentCount", ''#{type uint32_t},
		[| #{peek VkFramebufferCreateInfo, attachmentCount} |],
		[| #{poke VkFramebufferCreateInfo, attachmentCount} |]),
	("pAttachments", ''ImageView.PtrImageView,
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

foreign import ccall "vkCreateFramebuffer" create ::
	Device -> Ptr CreateInfo -> Ptr () -> Ptr Framebuffer ->
	IO #{type VkResult}

foreign import ccall "vkDestroyFramebuffer" destroy ::
	Device -> Framebuffer -> Ptr () -> IO ()
