{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.BindingFlags.Core (
	CreateInfo, pattern CreateInfo, BPtr,
	createInfoSType, createInfoPNext,
	createInfoBindingCount, createInfoPBindingFlags ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const
	VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO}

type BPtr = Ptr #{type VkDescriptorBindingFlags}

struct "CreateInfo" #{size VkDescriptorSetLayoutBindingFlagsCreateInfo}
		#{alignment VkDescriptorSetLayoutBindingFlagsCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ ->
			#{poke VkDescriptorSetLayoutBindingFlagsCreateInfo,
				sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDescriptorSetLayoutBindingFlagsCreateInfo,
			pNext} |],
		[| #{poke VkDescriptorSetLayoutBindingFlagsCreateInfo,
			pNext} |]),
	("bindingCount", ''#{type uint32_t},
		[| #{peek VkDescriptorSetLayoutBindingFlagsCreateInfo,
			bindingCount} |],
		[| #{poke VkDescriptorSetLayoutBindingFlagsCreateInfo,
			bindingCount} |]),
	("pBindingFlags", ''BPtr,
		[| #{peek VkDescriptorSetLayoutBindingFlagsCreateInfo,
			pBindingFlags} |],
		[| #{poke VkDescriptorSetLayoutBindingFlagsCreateInfo,
			pBindingFlags} |]) ]
	[''Show, ''Storable]
