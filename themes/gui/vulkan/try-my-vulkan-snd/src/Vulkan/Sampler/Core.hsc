{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Sampler.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

data STag
type S = Ptr STag

type PtrS = Ptr S

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO}

struct "CreateInfo" #{size VkSamplerCreateInfo}
		#{alignment VkSamplerCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSamplerCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkSamplerCreateInfo, pNext} |],
		[| #{poke VkSamplerCreateInfo, pNext} |]),
	("flags", ''#{type VkSamplerCreateFlags},
		[| #{peek VkSamplerCreateInfo, flags} |],
		[| #{poke VkSamplerCreateInfo, flags} |]),
	("magFilter", ''#{type VkFilter},
		[| #{peek VkSamplerCreateInfo, magFilter} |],
		[| #{poke VkSamplerCreateInfo, magFilter} |]),
	("minFilter", ''#{type VkFilter},
		[| #{peek VkSamplerCreateInfo, minFilter} |],
		[| #{poke VkSamplerCreateInfo, minFilter} |]),
	("mipmapMode", ''#{type VkSamplerMipmapMode},
		[| #{peek VkSamplerCreateInfo, mipmapMode} |],
		[| #{poke VkSamplerCreateInfo, mipmapMode} |]),
	("addressModeU", ''#{type VkSamplerAddressMode},
		[| #{peek VkSamplerCreateInfo, addressModeU} |],
		[| #{poke VkSamplerCreateInfo, addressModeU} |]),
	("addressModeV", ''#{type VkSamplerAddressMode},
		[| #{peek VkSamplerCreateInfo, addressModeV} |],
		[| #{poke VkSamplerCreateInfo, addressModeV} |]),
	("addressModeW", ''#{type VkSamplerAddressMode},
		[| #{peek VkSamplerCreateInfo, addressModeW} |],
		[| #{poke VkSamplerCreateInfo, addressModeW} |])
	]
	[''Show, ''Storable]
