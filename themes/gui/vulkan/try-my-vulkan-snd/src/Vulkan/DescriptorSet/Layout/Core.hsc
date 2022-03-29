{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.DescriptorSet.Layout.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

data LTag
type L = Ptr LTag

type PtrL = Ptr L

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO}

struct "CreateInfo" #{size VkDescriptorSetLayoutCreateInfo}
		#{alignment VkDescriptorSetLayoutCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkDescriptorSetLayoutCreateInfo, sType}
			p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDescriptorSetLayoutCreateInfo, pNext} |],
		[| #{poke VkDescriptorSetLayoutCreateInfo, pNext} |])
	]
	[''Show, ''Storable]
