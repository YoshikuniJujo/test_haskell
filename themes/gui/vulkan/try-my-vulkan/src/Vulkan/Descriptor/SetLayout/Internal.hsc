{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.SetLayout.Internal where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.DescriptorSetLayoutCreateFlagBits

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

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
		[| #{poke VkDescriptorSetLayoutCreateInfo, bindingCount} |])
	]
	[''Show, ''Storable]
