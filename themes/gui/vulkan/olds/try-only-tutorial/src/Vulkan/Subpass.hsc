{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Subpass where

import Foreign.Ptr
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

type PtrDescription = Ptr Description

struct "Dependency" #{size VkSubpassDependency}
		#{alignment VkSubpassDependency} [
	("srcSubpass", ''#{type uint32_t},
		[| #{peek VkSubpassDependency, srcSubpass} |],
		[| #{poke VkSubpassDependency, srcSubpass} |]),
	("dstSubpass", ''#{type uint32_t},
		[| #{peek VkSubpassDependency, dstSubpass} |],
		[| #{poke VkSubpassDependency, dstSubpass} |]),
	("srcStageMask", ''#{type VkPipelineStageFlags},
		[| #{peek VkSubpassDependency, srcStageMask} |],
		[| #{poke VkSubpassDependency, srcStageMask} |]),
	("dstStageMask", ''#{type VkPipelineStageFlags},
		[| #{peek VkSubpassDependency, dstStageMask} |],
		[| #{poke VkSubpassDependency, dstStageMask} |]),
	("srcAccessMask", ''#{type VkAccessFlags},
		[| #{peek VkSubpassDependency, srcAccessMask} |],
		[| #{poke VkSubpassDependency, srcAccessMask} |]),
	("dstAccessMask", ''#{type VkAccessFlags},
		[| #{peek VkSubpassDependency, dstAccessMask} |],
		[| #{poke VkSubpassDependency, dstAccessMask} |]),
	("dependencyFlags", ''#{type VkDependencyFlags},
		[| #{peek VkSubpassDependency, dependencyFlags} |],
		[| #{poke VkSubpassDependency, dependencyFlags} |]) ]
	[''Show, ''Storable]

type PtrDependency = Ptr Dependency

contentsInline :: #{type VkSubpassContents}
contentsInline = #{const VK_SUBPASS_CONTENTS_INLINE}

external :: #{type uint32_t}
external = #{const VK_SUBPASS_EXTERNAL}
