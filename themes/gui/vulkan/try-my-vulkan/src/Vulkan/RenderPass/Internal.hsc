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

import Vulkan.PipelineStageFlagBits
import Vulkan.AccessFlagBits
import Vulkan.DependencyFlagBits

import Vulkan.Framebuffer (Framebuffer)

import qualified Vulkan.StructureType as ST
import qualified Vulkan.Clear as Clear

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

type PtrAttachmentDescription = Ptr AttachmentDescription

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

type PtrSubpassDescription = Ptr SubpassDescription

struct "SubpassDependency" #{size VkSubpassDependency}
		#{alignment VkSubpassDependency} [
	("srcSubpass", ''#{type uint32_t},
		[| #{peek VkSubpassDependency, srcSubpass} |],
		[| #{poke VkSubpassDependency, srcSubpass} |]),
	("dstSubpass", ''#{type uint32_t},
		[| #{peek VkSubpassDependency, dstSubpass} |],
		[| #{poke VkSubpassDependency, dstSubpass} |]),
	("srcStageMask", ''PipelineStageFlags,
		[| #{peek VkSubpassDependency, srcStageMask} |],
		[| #{poke VkSubpassDependency, srcStageMask} |]),
	("dstStageMask", ''PipelineStageFlags,
		[| #{peek VkSubpassDependency, dstStageMask} |],
		[| #{poke VkSubpassDependency, dstStageMask} |]),
	("srcAccessMask", ''AccessFlags,
		[| #{peek VkSubpassDependency, srcAccessMask} |],
		[| #{poke VkSubpassDependency, srcAccessMask} |]),
	("dstAccessMask", ''AccessFlags,
		[| #{peek VkSubpassDependency, dstAccessMask} |],
		[| #{poke VkSubpassDependency, dstAccessMask} |]),
	("dependencyFlags", ''DependencyFlags,
		[| #{peek VkSubpassDependency, dependencyFlags} |],
		[| #{poke VkSubpassDependency, dependencyFlags} |])
	]
	[''Show, ''Storable]

type PtrSubpassDependency = Ptr SubpassDependency

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
		[| #{poke VkRenderPassCreateInfo, attachmentCount} |]),
	("pAttachments", ''PtrAttachmentDescription,
		[| #{peek VkRenderPassCreateInfo, pAttachments} |],
		[| #{poke VkRenderPassCreateInfo, pAttachments} |]),
	("subpassCount", ''#{type uint32_t},
		[| #{peek VkRenderPassCreateInfo, subpassCount} |],
		[| #{poke VkRenderPassCreateInfo, subpassCount} |]),
	("pSubpasses", ''PtrSubpassDescription,
		[| #{peek VkRenderPassCreateInfo, pSubpasses} |],
		[| #{poke VkRenderPassCreateInfo, pSubpasses} |]),
	("dependencyCount", ''#{type uint32_t},
		[| #{peek VkRenderPassCreateInfo, dependencyCount} |],
		[| #{poke VkRenderPassCreateInfo, dependencyCount} |]),
	("pDependencies", ''PtrSubpassDependency,
		[| #{peek VkRenderPassCreateInfo, pDependencies} |],
		[| #{poke VkRenderPassCreateInfo, pDependencies} |]) ]
	[''Show, ''Storable]

struct "BeginInfo" #{size VkRenderPassBeginInfo}
		#{alignment VkRenderPassBeginInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkRenderPassBeginInfo, sType}
			p ST.renderPassBeginInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkRenderPassBeginInfo, pNext} |],
		[| #{poke VkRenderPassBeginInfo, pNext} |]),
	("renderPass", ''RenderPass,
		[| #{peek VkRenderPassBeginInfo, renderPass} |],
		[| #{poke VkRenderPassBeginInfo, renderPass} |]),
	("framebuffer", ''Framebuffer,
		[| #{peek VkRenderPassBeginInfo, framebuffer} |],
		[| #{poke VkRenderPassBeginInfo, framebuffer} |]),
	("renderArea", ''Rect2d,
		[| #{peek VkRenderPassBeginInfo, renderArea} |],
		[| #{poke VkRenderPassBeginInfo, renderArea} |]),
	("clearValueCount", ''#{type uint32_t},
		[| #{peek VkRenderPassBeginInfo, clearValueCount} |],
		[| #{poke VkRenderPassBeginInfo, clearValueCount} |]),
	("pClearValues", ''Clear.PtrValue,
		[| #{peek VkRenderPassBeginInfo, pClearValues} |],
		[| #{poke VkRenderPassBeginInfo, pClearValues} |])
	]
	[''Show, ''Storable]
