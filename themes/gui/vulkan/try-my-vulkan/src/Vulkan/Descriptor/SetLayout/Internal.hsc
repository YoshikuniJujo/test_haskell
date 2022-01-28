{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.SetLayout.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.DescriptorType
import Vulkan.ShaderStageFlagBits
import Vulkan.Sampler (PtrSampler)
import Vulkan.DescriptorSetLayoutCreateFlagBits

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "Binding" #{size VkDescriptorSetLayoutBinding}
		#{alignment VkDescriptorSetLayoutBinding} [
	("binding", ''#{type uint32_t},
		[| #{peek VkDescriptorSetLayoutBinding, binding} |],
		[| #{poke VkDescriptorSetLayoutBinding, binding} |]),
	("descriptorType", ''DescriptorType,
		[| #{peek VkDescriptorSetLayoutBinding, descriptorType} |],
		[| #{poke VkDescriptorSetLayoutBinding, descriptorType} |]),
	("descriptorCount", ''#{type uint32_t},
		[| #{peek VkDescriptorSetLayoutBinding, descriptorCount} |],
		[| #{poke VkDescriptorSetLayoutBinding, descriptorCount} |]),
	("stageFlags", ''ShaderStageFlags,
		[| #{peek VkDescriptorSetLayoutBinding, stageFlags} |],
		[| #{poke VkDescriptorSetLayoutBinding, stageFlags} |]),
	("pImmutableSamplers", ''PtrSampler,
		[| #{peek VkDescriptorSetLayoutBinding, pImmutableSamplers} |],
		[| #{poke VkDescriptorSetLayoutBinding, pImmutableSamplers} |])
	]
	[''Show, ''Storable]

type PtrBinding = Ptr Binding

struct "CreateInfo" #{size VkDescriptorSetLayoutCreateInfo}
		#{alignment VkDescriptorSetLayoutCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDescriptorSetLayoutCreateInfo, sType} p
			ST.descriptorSetLayoutCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDescriptorSetLayoutCreateInfo, pNext} |],
		[| #{poke VkDescriptorSetLayoutCreateInfo, pNext} |]),
	("flags", ''DescriptorSetLayoutCreateFlags,
		[| #{peek VkDescriptorSetLayoutCreateInfo, flags} |],
		[| #{poke VkDescriptorSetLayoutCreateInfo, flags} |]),
	("bindingCount", ''#{type uint32_t},
		[| #{peek VkDescriptorSetLayoutCreateInfo, bindingCount} |],
		[| #{poke VkDescriptorSetLayoutCreateInfo, bindingCount} |]),
	("pBindings", ''PtrBinding,
		[| #{peek VkDescriptorSetLayoutCreateInfo, pBindings} |],
		[| #{poke VkDescriptorSetLayoutCreateInfo, pBindings} |])
	]
	[''Show, ''Storable]
