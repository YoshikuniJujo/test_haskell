{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Attachment.Core (

	-- * DESCRIPTION

	Description, PtrDescription, pattern Description,
	descriptionFlags, descriptionFormat, descriptionSamples,
	descriptionLoadOp, descriptionStoreOp,
	descriptionStencilLoadOp, descriptionStencilStoreOp,
	descriptionInitialLayout, descriptionFinalLayout,

	-- * REFERENCE

	Reference, PtrReference, pattern Reference,
	referenceAttachment, referenceLayout

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

struct "Description" #{size VkAttachmentDescription}
		#{alignment VkAttachmentDescription} [
	("flags", ''#{type VkAttachmentDescriptionFlags},
		[| #{peek VkAttachmentDescription, flags} |],
		[| #{poke VkAttachmentDescription, flags} |]),
	("format", ''#{type VkFormat},
		[| #{peek VkAttachmentDescription, format} |],
		[| #{poke VkAttachmentDescription, format} |]),
	("samples", ''#{type VkSampleCountFlagBits},
		[| #{peek VkAttachmentDescription, samples} |],
		[| #{poke VkAttachmentDescription, samples} |]),
	("loadOp", ''#{type VkAttachmentLoadOp},
		[| #{peek VkAttachmentDescription, loadOp} |],
		[| #{poke VkAttachmentDescription, loadOp} |]),
	("storeOp", ''#{type VkAttachmentStoreOp},
		[| #{peek VkAttachmentDescription, storeOp} |],
		[| #{poke VkAttachmentDescription, storeOp} |]),
	("stencilLoadOp", ''#{type VkAttachmentLoadOp},
		[| #{peek VkAttachmentDescription, stencilLoadOp} |],
		[| #{poke VkAttachmentDescription, stencilLoadOp} |]),
	("stencilStoreOp", ''#{type VkAttachmentStoreOp},
		[| #{peek VkAttachmentDescription, stencilStoreOp} |],
		[| #{poke VkAttachmentDescription, stencilStoreOp} |]),
	("initialLayout", ''#{type VkImageLayout},
		[| #{peek VkAttachmentDescription, initialLayout} |],
		[| #{poke VkAttachmentDescription, initialLayout} |]),
	("finalLayout", ''#{type VkImageLayout},
		[| #{peek VkAttachmentDescription, finalLayout} |],
		[| #{poke VkAttachmentDescription, finalLayout} |]) ]
	[''Show, ''Storable]

type PtrDescription = Ptr Description

struct "Reference" #{size VkAttachmentReference}
		#{alignment VkAttachmentReference} [
	("attachment", ''#{type uint32_t},
		[| #{peek VkAttachmentReference, attachment} |],
		[| #{poke VkAttachmentReference, attachment} |]),
	("layout", ''#{type VkImageLayout},
		[| #{peek VkAttachmentReference, layout} |],
		[| #{poke VkAttachmentReference, layout} |]) ]
	[''Show, ''Storable]

type PtrReference = Ptr Reference
