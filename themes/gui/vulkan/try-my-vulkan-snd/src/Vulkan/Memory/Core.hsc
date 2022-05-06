{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory.Core where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

struct "Requirements" #{size VkMemoryRequirements}
		#{alignment VkMemoryRequirements} [
	("size", ''#{type VkDeviceSize},
		[| #{peek VkMemoryRequirements, size} |],
		[| #{poke VkMemoryRequirements, size} |]),
	("alignment", ''#{type VkDeviceSize},
		[| #{peek VkMemoryRequirements, alignment} |],
		[| #{poke VkMemoryRequirements, alignment} |]),
	("memoryTypeBits", ''#{type uint32_t},
		[| #{peek VkMemoryRequirements, memoryTypeBits} |],
		[| #{poke VkMemoryRequirements, memoryTypeBits} |]) ]
	[''Show, ''Storable]
