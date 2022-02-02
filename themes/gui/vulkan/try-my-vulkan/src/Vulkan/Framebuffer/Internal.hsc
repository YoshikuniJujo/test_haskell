{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Framebuffer.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.FramebufferCreateFlagBits
import Vulkan.Image (PtrImageView)

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkFramebufferCreateInfo}
		#{alignment VkFramebufferCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkFramebufferCreateInfo, sType}
			p ST.framebufferCreateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkFramebufferCreateInfo, pNext} |],
		[| #{poke VkFramebufferCreateInfo, pNext} |]),
	("flags", ''FramebufferCreateFlags,
		[| #{peek VkFramebufferCreateInfo, flags} |],
		[| #{poke VkFramebufferCreateInfo, flags} |]),
	("renderPass", ''RenderPass,
		[| #{peek VkFramebufferCreateInfo, renderPass} |],
		[| #{poke VkFramebufferCreateInfo, renderPass} |]),
	("attachmentCount", ''#{type uint32_t},
		[| #{peek VkFramebufferCreateInfo, attachmentCount} |],
		[| #{poke VkFramebufferCreateInfo, attachmentCount} |]),
	("pAttachments", ''PtrImageView,
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
