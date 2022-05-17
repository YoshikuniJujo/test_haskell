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
