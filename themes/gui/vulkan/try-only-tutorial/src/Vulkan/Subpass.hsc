{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Subpass where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

struct "Description" #{size VkSubpassDescription}
		#{alignment VkSubpassDescription} [
	("flags", ''#{type VkSubpassDescriptionFlags},
		[| #{peek VkSubpassDescription, flags} |],
		[| #{poke VkSubpassDescription, flags} |])
	]
	[''Show, ''Storable]
