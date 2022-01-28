{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Sampler.Internal where

import Foreign.Storable
import Foreign.C.Struct

import Vulkan.Base
import Vulkan.SamplerCreateFlagBits

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkSamplerCreateInfo}
		#{alignment VkSamplerCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSamplerCreateInfo, sType} p
			ST.samplerCreateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkSamplerCreateInfo, pNext} |],
		[| #{poke VkSamplerCreateInfo, pNext} |]),
	("flags", ''SamplerCreateFlags,
		[| #{peek VkSamplerCreateInfo, flags} |],
		[| #{poke VkSamplerCreateInfo, flags} |])
	]
	[''Show, ''Storable]
