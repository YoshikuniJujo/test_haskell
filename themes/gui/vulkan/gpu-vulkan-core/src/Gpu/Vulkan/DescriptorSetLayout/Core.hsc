{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSetLayout.Core (
	-- * CREATE AND DESTROY

	create, destroy, D, PtrD, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoBindingCount, createInfoPBindings,

	-- ** Binding
	Binding, PtrBinding, pattern Binding,
	bindingBinding, bindingDescriptorType, bindingDescriptorCount,
	bindingStageFlags, bindingPImmutableSamplers

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.Sampler.Core as Sampler

#include <vulkan/vulkan.h>

data DTag
type D = Ptr DTag

type PtrD = Ptr D

struct "Binding" #{size VkDescriptorSetLayoutBinding}
		#{alignment VkDescriptorSetLayoutBinding} [
	("binding", ''#{type uint32_t},
		[| #{peek VkDescriptorSetLayoutBinding, binding} |],
		[| #{poke VkDescriptorSetLayoutBinding, binding} |]),
	("descriptorType", ''#{type VkDescriptorType},
		[| #{peek VkDescriptorSetLayoutBinding, descriptorType} |],
		[| #{poke VkDescriptorSetLayoutBinding, descriptorType} |]),
	("descriptorCount", ''#{type uint32_t},
		[| #{peek VkDescriptorSetLayoutBinding, descriptorCount} |],
		[| #{poke VkDescriptorSetLayoutBinding, descriptorCount} |]),
	("stageFlags", ''#{type VkShaderStageFlags},
		[| #{peek VkDescriptorSetLayoutBinding, stageFlags} |],
		[| #{poke VkDescriptorSetLayoutBinding, stageFlags} |]),
	("pImmutableSamplers", ''Sampler.PtrS,
		[| #{peek VkDescriptorSetLayoutBinding, pImmutableSamplers} |],
		[| #{poke VkDescriptorSetLayoutBinding, pImmutableSamplers} |])
	]
	[''Show, ''Storable]

type PtrBinding = Ptr Binding

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO}

struct "CreateInfo" #{size VkDescriptorSetLayoutCreateInfo}
		#{alignment VkDescriptorSetLayoutCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDescriptorSetLayoutCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDescriptorSetLayoutCreateInfo, pNext} |],
		[| #{poke VkDescriptorSetLayoutCreateInfo, pNext} |]),
	("flags", ''#{type VkDescriptorSetLayoutCreateFlags},
		[| #{peek VkDescriptorSetLayoutCreateInfo, flags} |],
		[| #{poke VkDescriptorSetLayoutCreateInfo, flags} |]),
	("bindingCount", ''#{type uint32_t},
		[| #{peek VkDescriptorSetLayoutCreateInfo, bindingCount} |],
		[| #{poke VkDescriptorSetLayoutCreateInfo, bindingCount} |]),
	("pBindings", ''PtrBinding,
		[| #{peek VkDescriptorSetLayoutCreateInfo, pBindings} |],
		[| #{poke VkDescriptorSetLayoutCreateInfo, pBindings} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateDescriptorSetLayout" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr D ->
	IO #{type VkResult}

foreign import ccall "vkDestroyDescriptorSetLayout" destroy ::
	Device.D -> D -> Ptr AllocationCallbacks.A -> IO ()
