{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Attachment where

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

loadOpClear, loadOpDontCare :: #{type VkAttachmentLoadOp}
loadOpClear = #{const VK_ATTACHMENT_LOAD_OP_CLEAR}
loadOpDontCare = #{const VK_ATTACHMENT_LOAD_OP_DONT_CARE}

storeOpStore, storeOpDontCare :: #{type VkAttachmentStoreOp}
storeOpStore = #{const VK_ATTACHMENT_STORE_OP_STORE}
storeOpDontCare = #{const VK_ATTACHMENT_STORE_OP_DONT_CARE}
