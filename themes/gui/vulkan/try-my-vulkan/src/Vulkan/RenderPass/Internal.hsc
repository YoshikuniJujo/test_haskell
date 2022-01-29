{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.AttachmentDescriptionFlagBits
import Vulkan.Format
import Vulkan.SampleCountFlagBits
import Vulkan.AttachmentLoadOp
import Vulkan.AttachmentStoreOp
import Vulkan.ImageLayout
import Vulkan.SubpassDescriptionFlagBits
import Vulkan.PipelineBindPoint

#include <vulkan/vulkan.h>

struct "AttachmentDescription" #{size VkAttachmentDescription}
		#{alignment VkAttachmentDescription} [
	("flags", ''AttachmentDescriptionFlags,
		[| #{peek VkAttachmentDescription, flags} |],
		[| #{poke VkAttachmentDescription, flags} |]),
	("format", ''Format, [| #{peek VkAttachmentDescription, format} |],
		[| #{poke VkAttachmentDescription, format} |]),
	("samples", ''SampleCountFlagBits,
		[| #{peek VkAttachmentDescription, samples} |],
		[| #{poke VkAttachmentDescription, samples} |]),
	("loadOp", ''AttachmentLoadOp,
		[| #{peek VkAttachmentDescription, loadOp} |],
		[| #{poke VkAttachmentDescription, loadOp} |]),
	("storeOp", ''AttachmentStoreOp,
		[| #{peek VkAttachmentDescription, storeOp} |],
		[| #{poke VkAttachmentDescription, storeOp} |]),
	("stencilLoadOp", ''AttachmentLoadOp,
		[| #{peek VkAttachmentDescription, stencilLoadOp} |],
		[| #{poke VkAttachmentDescription, stencilLoadOp} |]),
	("stencilStoreOp", ''AttachmentStoreOp,
		[| #{peek VkAttachmentDescription, stencilStoreOp} |],
		[| #{poke VkAttachmentDescription, stencilStoreOp} |]),
	("initialLayout", ''ImageLayout,
		[| #{peek VkAttachmentDescription, initialLayout} |],
		[| #{poke VkAttachmentDescription, initialLayout} |]),
	("finalLayout", ''ImageLayout,
		[| #{peek VkAttachmentDescription, finalLayout} |],
		[| #{poke VkAttachmentDescription, finalLayout} |]) ]
	[''Show, ''Storable]

struct "AttachmentReference" #{size VkAttachmentReference}
		#{alignment VkAttachmentReference} [
	("attachment", ''#{type uint32_t},
		[| #{peek VkAttachmentReference, attachment} |],
		[| #{poke VkAttachmentReference, attachment} |]),
	("layout", ''ImageLayout,
		[| #{peek VkAttachmentReference, layout} |],
		[| #{poke VkAttachmentReference, layout} |]) ]
	[''Show, ''Storable]

struct "SubpassDescription" #{size VkSubpassDescription}
		#{alignment VkSubpassDescription} [
	("flags", ''SubpassDescriptionFlags,
		[| #{peek VkSubpassDescription, flags} |],
		[| #{poke VkSubpassDescription, flags} |]),
	("pipelineBindPoint", ''PipelineBindPoint,
		[| #{peek VkSubpassDescription, pipelineBindPoint} |],
		[| #{poke VkSubpassDescription, pipelineBindPoint} |])
	]
	[''Show, ''Storable]
