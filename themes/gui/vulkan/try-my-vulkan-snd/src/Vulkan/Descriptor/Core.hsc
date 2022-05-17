{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Core where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import qualified Vulkan.Buffer.Core as Buffer

#include <vulkan/vulkan.h>

struct "BufferInfo" #{size VkDescriptorBufferInfo}
		#{alignment VkDescriptorBufferInfo} [
	("buffer", ''Buffer.B,
		[| #{peek VkDescriptorBufferInfo, buffer} |],
		[| #{poke VkDescriptorBufferInfo, buffer} |]),
	("offset", ''#{type VkDeviceSize},
		[| #{peek VkDescriptorBufferInfo, offset} |],
		[| #{poke VkDescriptorBufferInfo, offset} |]),
	("range", ''#{type VkDeviceSize},
		[| #{peek VkDescriptorBufferInfo, range} |],
		[| #{poke VkDescriptorBufferInfo, range} |]) ]
	[''Show, ''Storable]
