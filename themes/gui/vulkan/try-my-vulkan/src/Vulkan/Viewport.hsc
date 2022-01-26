{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Viewport where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct

#include <vulkan/vulkan.h>

struct "Viewport" #{size VkViewport} #{alignment VkViewport} [
	("x", ''#{type float},
		[| #{peek VkViewport, x} |], [| #{poke VkViewport, x} |]),
	("y", ''#{type float},
		[| #{peek VkViewport, y} |], [| #{poke VkViewport, y} |]),
	("width", ''#{type float}, [| #{peek VkViewport, width} |],
		[| #{poke VkViewport, width} |]),
	("height", ''#{type float}, [| #{peek VkViewport, height} |],
		[| #{poke VkViewport, height} |]),
	("minDepth", ''#{type float}, [| #{peek VkViewport, minDepth} |],
		[| #{poke VkViewport, minDepth} |]),
	("maxDepth", ''#{type float}, [| #{peek VkViewport, maxDepth} |],
		[| #{poke VkViewport, maxDepth} |]) ]
	[''Show, ''Storable]

type PtrViewport = Ptr Viewport
