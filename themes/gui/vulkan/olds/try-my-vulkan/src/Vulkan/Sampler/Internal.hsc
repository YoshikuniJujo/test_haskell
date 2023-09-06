{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Sampler.Internal where

import Foreign.Storable
import Foreign.C.Struct

import Vulkan.Base
import Vulkan.SamplerCreateFlagBits
import Vulkan.Filter
import Vulkan.SamplerMipmapMode
import Vulkan.SamplerAddressMode
import Vulkan.CompareOp
import Vulkan.BorderColor

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
		[| #{poke VkSamplerCreateInfo, flags} |]),
	("magFilter", ''Filter, [| #{peek VkSamplerCreateInfo, magFilter} |],
		[| #{poke VkSamplerCreateInfo, magFilter} |]),
	("minFilter", ''Filter, [| #{peek VkSamplerCreateInfo, minFilter} |],
		[| #{poke VkSamplerCreateInfo, minFilter} |]),
	("mipmapMode", ''SamplerMipmapMode,
		[| #{peek VkSamplerCreateInfo, mipmapMode} |],
		[| #{poke VkSamplerCreateInfo, mipmapMode} |]),
	("addressModeU", ''SamplerAddressMode,
		[| #{peek VkSamplerCreateInfo, addressModeU} |],
		[| #{poke VkSamplerCreateInfo, addressModeU} |]),
	("addressModeV", ''SamplerAddressMode,
		[| #{peek VkSamplerCreateInfo, addressModeV} |],
		[| #{poke VkSamplerCreateInfo, addressModeV} |]),
	("addressModeW", ''SamplerAddressMode,
		[| #{peek VkSamplerCreateInfo, addressModeW} |],
		[| #{poke VkSamplerCreateInfo, addressModeW} |]),
	("mipLodBias", ''#{type float},
		[| #{peek VkSamplerCreateInfo, mipLodBias} |],
		[| #{poke VkSamplerCreateInfo, mipLodBias} |]),
	("anisotropyEnable", ''Bool32,
		[| #{peek VkSamplerCreateInfo, anisotropyEnable} |],
		[| #{poke VkSamplerCreateInfo, anisotropyEnable} |]),
	("maxAnisotropy", ''#{type float},
		[| #{peek VkSamplerCreateInfo, maxAnisotropy} |],
		[| #{poke VkSamplerCreateInfo, maxAnisotropy} |]),
	("compareEnable", ''Bool32,
		[| #{peek VkSamplerCreateInfo, compareEnable} |],
		[| #{poke VkSamplerCreateInfo, compareEnable} |]),
	("compareOp", ''CompareOp,
		[| #{peek VkSamplerCreateInfo, compareOp} |],
		[| #{poke VkSamplerCreateInfo, compareOp} |]),
	("minLod", ''#{type float},
		[| #{peek VkSamplerCreateInfo, minLod} |],
		[| #{poke VkSamplerCreateInfo, minLod} |]),
	("maxLod", ''#{type float},
		[| #{peek VkSamplerCreateInfo, maxLod} |],
		[| #{poke VkSamplerCreateInfo, maxLod} |]),
	("borderColor", ''BorderColor,
		[| #{peek VkSamplerCreateInfo, borderColor} |],
		[| #{poke VkSamplerCreateInfo, borderColor} |]),
	("unnormalizedCoordinates", ''Bool32,
		[| #{peek VkSamplerCreateInfo, unnormalizedCoordinates} |],
		[| #{poke VkSamplerCreateInfo, unnormalizedCoordinates} |]) ]
	[''Show, ''Storable]
