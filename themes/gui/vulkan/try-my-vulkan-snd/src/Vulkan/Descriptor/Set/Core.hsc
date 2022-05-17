{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Set.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word
import Data.Int

import Vulkan.Base

import qualified Vulkan.Device.Core as Device
import qualified Vulkan.Buffer.View.Core as Buffer.View
import qualified Vulkan.Descriptor.Core as Dsc
import qualified Vulkan.Descriptor.Pool.Core as Pool
import qualified Vulkan.Descriptor.Set.Layout.Core as Layout

#include <vulkan/vulkan.h>

aType :: #{type VkStructureType}
aType = #{const VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO}

struct "AllocateInfo" #{size VkDescriptorSetAllocateInfo}
		#{alignment VkDescriptorSetAllocateInfo} [
	("sType", ''(), [| const $ pure () |], [| \p _ ->
		#{poke VkDescriptorSetAllocateInfo, sType} p aType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDescriptorSetAllocateInfo, pNext} |],
		[| #{poke VkDescriptorSetAllocateInfo, pNext} |]),
	("descriptorPool", ''Pool.P,
		[| #{peek VkDescriptorSetAllocateInfo, descriptorPool} |],
		[| #{poke VkDescriptorSetAllocateInfo, descriptorPool} |]),
	("descriptorSetCount", ''#{type uint32_t},
		[| #{peek VkDescriptorSetAllocateInfo, descriptorSetCount} |],
		[| #{poke VkDescriptorSetAllocateInfo, descriptorSetCount} |]),
	("pSetLayouts", ''Layout.PtrL,
		[| #{peek VkDescriptorSetAllocateInfo, pSetLayouts} |],
		[| #{poke VkDescriptorSetAllocateInfo, pSetLayouts} |]) ]
	[''Show, ''Storable]

data STag
type S = Ptr STag

foreign import ccall "vkAllocateDescriptorSets" allocateSs ::
	Device.D -> Ptr AllocateInfo -> Ptr S -> IO #{type VkResult}

wType :: #{type VkStructureType}
wType = #{const VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET}

struct "Write" #{size VkWriteDescriptorSet} #{alignment VkWriteDescriptorSet} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkWriteDescriptorSet, sType} p wType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkWriteDescriptorSet, pNext} |],
		[| #{poke VkWriteDescriptorSet, pNext} |]),
	("dstSet", ''S,
		[| #{peek VkWriteDescriptorSet, dstSet} |],
		[| #{poke VkWriteDescriptorSet, dstSet} |]),
	("dstBinding", ''#{type uint32_t},
		[| #{peek VkWriteDescriptorSet, dstBinding} |],
		[| #{poke VkWriteDescriptorSet, dstBinding} |]),
	("dstArrayElement", ''#{type uint32_t},
		[| #{peek VkWriteDescriptorSet, dstArrayElement} |],
		[| #{poke VkWriteDescriptorSet, dstArrayElement} |]),
	("descriptorCount", ''#{type uint32_t},
		[| #{peek VkWriteDescriptorSet, descriptorCount} |],
		[| #{poke VkWriteDescriptorSet, descriptorCount} |]),
	("descriptorType", ''#{type VkDescriptorType},
		[| #{peek VkWriteDescriptorSet, descriptorType} |],
		[| #{poke VkWriteDescriptorSet, descriptorType} |]),
	("pImageInfo", ''Dsc.PtrImageInfo,
		[| #{peek VkWriteDescriptorSet, pImageInfo} |],
		[| #{poke VkWriteDescriptorSet, pImageInfo} |]),
	("pBufferInfo", ''Dsc.PtrBufferInfo,
		[| #{peek VkWriteDescriptorSet, pBufferInfo} |],
		[| #{poke VkWriteDescriptorSet, pBufferInfo} |]),
	("pTexelBufferView", ''Buffer.View.PtrV,
		[| #{peek VkWriteDescriptorSet, pTexelBufferView} |],
		[| #{poke VkWriteDescriptorSet, pTexelBufferView} |]) ]
	[''Show, ''Storable]
