{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Component.Core (
	Mapping, pattern Mapping, mappingR, mappingG, mappingB, mappingA,
	swizzleIdentity ) where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

struct "Mapping" #{size VkComponentMapping}
		#{alignment VkComponentMapping} [
	("r", ''#{type VkComponentSwizzle},
		[| #{peek VkComponentMapping, r} |],
		[| #{poke VkComponentMapping, r} |]),
	("g", ''#{type VkComponentSwizzle},
		[| #{peek VkComponentMapping, g} |],
		[| #{poke VkComponentMapping, g} |]),
	("b", ''#{type VkComponentSwizzle},
		[| #{peek VkComponentMapping, b} |],
		[| #{poke VkComponentMapping, b} |]),
	("a", ''#{type VkComponentSwizzle},
		[| #{peek VkComponentMapping, a} |],
		[| #{poke VkComponentMapping, a} |]) ]
	[''Show, ''Storable]

swizzleIdentity :: #{type VkComponentSwizzle}
swizzleIdentity = #{const VK_COMPONENT_SWIZZLE_IDENTITY}
