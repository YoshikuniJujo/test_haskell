{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Core (

	-- * ALLOCATE AND FREE

	allocateDs, freeDs, D, AllocateInfo, pattern AllocateInfo,
	allocateInfoSType, allocateInfoPNext,
	allocateInfoDescriptorPool, allocateInfoDescriptorSetCount,
	allocateInfoPSetLayouts,

	-- * UPDATE

	updateDs,

	-- ** Write

	Write, pattern Write,
	writeSType, writePNext,
	writeDstSet, writeDstBinding, writeDstArrayElement,
	writeDescriptorCount, writeDescriptorType,
	writePImageInfo, writePBufferInfo, writePTexelBufferView,

	-- ** Copy

	Copy, pattern Copy,
	copySType, copyPNext,
	copySrcSet, copySrcBinding, copySrcArrayElement,
	copyDstSet, copyDstBinding, copyDstArrayElement,
	copyDescriptorCount

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.Device.Core as Device
import qualified Gpu.Vulkan.BufferView.Core as Buffer.View
import qualified Gpu.Vulkan.Descriptor.Core as Dsc
import qualified Gpu.Vulkan.DescriptorPool.Core as Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Core as Layout

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
	("descriptorPool", ''Pool.D,
		[| #{peek VkDescriptorSetAllocateInfo, descriptorPool} |],
		[| #{poke VkDescriptorSetAllocateInfo, descriptorPool} |]),
	("descriptorSetCount", ''#{type uint32_t},
		[| #{peek VkDescriptorSetAllocateInfo, descriptorSetCount} |],
		[| #{poke VkDescriptorSetAllocateInfo, descriptorSetCount} |]),
	("pSetLayouts", ''Layout.PtrD,
		[| #{peek VkDescriptorSetAllocateInfo, pSetLayouts} |],
		[| #{poke VkDescriptorSetAllocateInfo, pSetLayouts} |]) ]
	[''Show, ''Storable]

data DTag
type D = Ptr DTag

foreign import ccall "vkAllocateDescriptorSets" allocateDs ::
	Device.D -> Ptr AllocateInfo -> Ptr D -> IO #{type VkResult}

foreign import ccall "vkFreeDescriptorSets" freeDs ::
	Device.D -> Pool.D -> #{type uint32_t} -> Ptr D -> IO #{type VkResult}

wType :: #{type VkStructureType}
wType = #{const VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET}

struct "Write" #{size VkWriteDescriptorSet} #{alignment VkWriteDescriptorSet} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkWriteDescriptorSet, sType} p wType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkWriteDescriptorSet, pNext} |],
		[| #{poke VkWriteDescriptorSet, pNext} |]),
	("dstSet", ''D,
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
	("pTexelBufferView", ''Buffer.View.PtrB,
		[| #{peek VkWriteDescriptorSet, pTexelBufferView} |],
		[| #{poke VkWriteDescriptorSet, pTexelBufferView} |]) ]
	[''Show, ''Storable]

cType :: #{type VkStructureType}
cType = #{const VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET}

struct "Copy" #{size VkCopyDescriptorSet} #{alignment VkCopyDescriptorSet} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkCopyDescriptorSet, sType} p cType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkCopyDescriptorSet, pNext} |],
		[| #{poke VkCopyDescriptorSet, pNext} |]),
	("srcSet", ''D,
		[| #{peek VkCopyDescriptorSet, srcSet} |],
		[| #{poke VkCopyDescriptorSet, srcSet} |]),
	("srcBinding", ''#{type uint32_t},
		[| #{peek VkCopyDescriptorSet, srcBinding} |],
		[| #{poke VkCopyDescriptorSet, srcBinding} |]),
	("srcArrayElement", ''#{type uint32_t},
		[| #{peek VkCopyDescriptorSet, srcArrayElement} |],
		[| #{poke VkCopyDescriptorSet, srcArrayElement} |]),
	("dstSet", ''D,
		[| #{peek VkCopyDescriptorSet, dstSet} |],
		[| #{poke VkCopyDescriptorSet, dstSet} |]),
	("dstBinding", ''#{type uint32_t},
		[| #{peek VkCopyDescriptorSet, dstBinding} |],
		[| #{poke VkCopyDescriptorSet, dstBinding} |]),
	("dstArrayElement", ''#{type uint32_t},
		[| #{peek VkCopyDescriptorSet, dstArrayElement} |],
		[| #{poke VkCopyDescriptorSet, dstArrayElement} |]),
	("descriptorCount", ''#{type uint32_t},
		[| #{peek VkCopyDescriptorSet, descriptorCount} |],
		[| #{poke VkCopyDescriptorSet, descriptorCount} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkUpdateDescriptorSets" updateDs ::
	Device.D ->
	#{type uint32_t} -> Ptr Write -> #{type uint32_t} -> Ptr Copy -> IO ()
