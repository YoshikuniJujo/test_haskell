{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Subpass where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

import qualified Vulkan.Attachment as Attachment

#include <vulkan/vulkan.h>

struct "Description" #{size VkSubpassDescription}
		#{alignment VkSubpassDescription} [
	("flags", ''#{type VkSubpassDescriptionFlags},
		[| #{peek VkSubpassDescription, flags} |],
		[| #{poke VkSubpassDescription, flags} |]),
	("pipelineBindPoint", ''#{type VkPipelineBindPoint},
		[| #{peek VkSubpassDescription, pipelineBindPoint} |],
		[| #{poke VkSubpassDescription, pipelineBindPoint} |]),
	("inputAttachmentCount", ''#{type uint32_t},
		[| #{peek VkSubpassDescription, inputAttachmentCount} |],
		[| #{poke VkSubpassDescription, inputAttachmentCount} |]),
	("pInputAttachments", ''Attachment.PtrReference,
		[| #{peek VkSubpassDescription, pInputAttachments} |],
		[| #{poke VkSubpassDescription, pInputAttachments} |]),
	("colorAttachmentCount", ''#{type uint32_t},
		[| #{peek VkSubpassDescription, colorAttachmentCount} |],
		[| #{poke VkSubpassDescription, colorAttachmentCount} |]),
	("pColorAttachments", ''Attachment.PtrReference,
		[| #{peek VkSubpassDescription, pColorAttachments} |],
		[| #{poke VkSubpassDescription, pColorAttachments} |]),
	("pResolveAttachments", ''Attachment.PtrReference,
		[| #{peek VkSubpassDescription, pResolveAttachments} |],
		[| #{poke VkSubpassDescription, pResolveAttachments} |]),
	("pDepthStencilAttachment", ''Attachment.PtrReference,
		[| #{peek VkSubpassDescription, pDepthStencilAttachment} |],
		[| #{poke VkSubpassDescription, pDepthStencilAttachment} |]),
	("preserveAttachmentCount", ''#{type uint32_t},
		[| #{peek VkSubpassDescription, preserveAttachmentCount} |],
		[| #{poke VkSubpassDescription, preserveAttachmentCount} |]),
	("pPreserveAttachments", ''PtrUint32T,
		[| #{peek VkSubpassDescription, pPreserveAttachments} |],
		[| #{poke VkSubpassDescription, pPreserveAttachments} |]) ]
	[''Show, ''Storable]
