{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Format.Core where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

struct "FormatProperties" #{size VkFormatProperties}
		#{alignment VkFormatProperties} [
	("linearTilingFeatures", ''#{type VkFormatFeatureFlags},
		[| #{peek VkFormatProperties, linearTilingFeatures} |],
		[| #{poke VkFormatProperties, linearTilingFeatures} |]),
	("optimalTilingFeatures", ''#{type VkFormatFeatureFlags},
		[| #{peek VkFormatProperties, optimalTilingFeatures} |],
		[| #{poke VkFormatProperties, optimalTilingFeatures} |]),
	("bufferFeatures", ''#{type VkFormatFeatureFlags},
		[| #{peek VkFormatProperties, bufferFeatures} |],
		[| #{poke VkFormatProperties, bufferFeatures} |]) ]
	[''Show, ''Storable]
