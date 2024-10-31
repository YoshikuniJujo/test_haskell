{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.VertexInput.Core (

	-- * BINDING DESCRIPTION

	BindingDescription, PtrBindingDescription, pattern BindingDescription,
	bindingDescriptionBinding, bindingDescriptionStride,
	bindingDescriptionInputRate,

	-- * ATTRIBUTE DESCRIPTION

	AttributeDescription, PtrAttributeDescription,
	pattern AttributeDescription,
	attributeDescriptionLocation, attributeDescriptionBinding,
	attributeDescriptionFormat, attributeDescriptionOffset

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

struct "BindingDescription" #{size VkVertexInputBindingDescription}
		#{alignment VkVertexInputBindingDescription} [
	("binding", ''#{type uint32_t},
		[| #{peek VkVertexInputBindingDescription, binding} |],
		[| #{poke VkVertexInputBindingDescription, binding} |]),
	("stride", ''#{type uint32_t},
		[| #{peek VkVertexInputBindingDescription, stride} |],
		[| #{poke VkVertexInputBindingDescription, stride} |]),
	("inputRate", ''#{type VkVertexInputRate},
		[| #{peek VkVertexInputBindingDescription, inputRate} |],
		[| #{poke VkVertexInputBindingDescription, inputRate} |]) ]
	[''Show, ''Storable]

type PtrBindingDescription = Ptr BindingDescription

struct "AttributeDescription" #{size VkVertexInputAttributeDescription}
		#{alignment VkVertexInputAttributeDescription} [
	("location", ''#{type uint32_t},
		[| #{peek VkVertexInputAttributeDescription, location} |],
		[| #{poke VkVertexInputAttributeDescription, location} |]),
	("binding", ''#{type uint32_t},
		[| #{peek VkVertexInputAttributeDescription, binding} |],
		[| #{poke VkVertexInputAttributeDescription, binding} |]),
	("format", ''#{type VkFormat},
		[| #{peek VkVertexInputAttributeDescription, format} |],
		[| #{poke VkVertexInputAttributeDescription, format} |]),
	("offset", ''#{type uint32_t},
		[| #{peek VkVertexInputAttributeDescription, offset} |],
		[| #{poke VkVertexInputAttributeDescription, offset} |]) ]
	[''Show, ''Storable]

type PtrAttributeDescription = Ptr AttributeDescription
