{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Pool.Core where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

struct "Size" #{size VkDescriptorPoolSize} #{alignment VkDescriptorPoolSize} [
	("type", ''#{type VkDescriptorType},
		[| #{peek VkDescriptorPoolSize, type} |],
		[| #{poke VkDescriptorPoolSize, type} |]),
	("descriptorCount", ''#{type uint32_t},
		[| #{peek VkDescriptorPoolSize, descriptorCount} |],
		[| #{poke VkDescriptorPoolSize, descriptorCount} |])
	]
	[''Show, ''Storable]
