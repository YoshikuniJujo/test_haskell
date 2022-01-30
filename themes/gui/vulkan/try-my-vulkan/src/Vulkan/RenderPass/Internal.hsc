{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.AttachmentDescriptionFlagBits
import Vulkan.Format
import Vulkan.SampleCountFlagBits
import Vulkan.AttachmentLoadOp
import Vulkan.AttachmentStoreOp
import Vulkan.ImageLayout
import Vulkan.SubpassDescriptionFlagBits
import Vulkan.PipelineBindPoint
import Vulkan.RenderPassCreateFlagBits

import qualified Vulkan.StructureType as ST

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

type PtrAttachmentReference = Ptr AttachmentReference

struct "SubpassDescription" #{size VkSubpassDescription}
		#{alignment VkSubpassDescription} [
	("flags", ''SubpassDescriptionFlags,
		[| #{peek VkSubpassDescription, flags} |],
		[| #{poke VkSubpassDescription, flags} |]),
	("pipelineBindPoint", ''PipelineBindPoint,
		[| #{peek VkSubpassDescription, pipelineBindPoint} |],
		[| #{poke VkSubpassDescription, pipelineBindPoint} |]),
	("inputAttachmentCount", ''#{type uint32_t},
		[| #{peek VkSubpassDescription, inputAttachmentCount} |],
		[| #{poke VkSubpassDescription, inputAttachmentCount} |]),
	("pInputAttachments", ''PtrAttachmentReference,
		[| #{peek VkSubpassDescription, pInputAttachments} |],
		[| #{poke VkSubpassDescription, pInputAttachments} |]),
	("colorAttachmentCount", ''#{type uint32_t},
		[| #{peek VkSubpassDescription, colorAttachmentCount} |],
		[| #{poke VkSubpassDescription, colorAttachmentCount} |]),
	("pColorAttachments", ''PtrAttachmentReference,
		[| #{peek VkSubpassDescription, pColorAttachments} |],
		[| #{poke VkSubpassDescription, pColorAttachments} |]),
	("pResolveAttachments", ''PtrAttachmentReference,
		[| #{peek VkSubpassDescription, pResolveAttachments} |],
		[| #{poke VkSubpassDescription, pResolveAttachments} |]),
	("pDepthStencilAttachment", ''PtrAttachmentReference,
		[| #{peek VkSubpassDescription, pDepthStencilAttachment} |],
		[| #{poke VkSubpassDescription, pDepthStencilAttachment} |]),
	("preserveAttachmentCount", ''#{type uint32_t},
		[| #{peek VkSubpassDescription, preserveAttachmentCount} |],
		[| #{poke VkSubpassDescription, preserveAttachmentCount} |]),
	("pPreserveAttachments", ''PtrUint32T,
		[| #{peek VkSubpassDescription, pPreserveAttachments} |],
		[| #{poke VkSubpassDescription, pPreserveAttachments} |]) ]
	[''Show, ''Storable]

struct "CreateInfo" #{size VkRenderPassCreateInfo}
		#{alignment VkRenderPassCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkRenderPassCreateInfo, sType} p
			ST.renderPassCreateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkRenderPassCreateInfo, pNext} |],
		[| #{poke VkRenderPassCreateInfo, pNext} |]),
	("flags", ''RenderPassCreateFlags,
		[| #{peek VkRenderPassCreateInfo, flags} |],
		[| #{poke VkRenderPassCreateInfo, flags} |]),
	("attachmentCount", ''#{type uint32_t},
		[| #{peek VkRenderPassCreateInfo, attachmentCount} |],
		[| #{poke VkRenderPassCreateInfo, attachmentCount} |])
	]
	[''Show, ''Storable]
