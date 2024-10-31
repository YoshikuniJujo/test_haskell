{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sampler.Core (

	-- * CREATE AND DESTROY

	create, destroy, S, PtrS, CreateInfo, pattern CreateInfo,
	createInfoSType, createInfoPNext, createInfoFlags,
	createInfoMagFilter, createInfoMinFilter, createInfoMipmapMode,
	createInfoAddressModeU, createInfoAddressModeV, createInfoAddressModeW,
	createInfoMipLodBias,
	createInfoAnisotropyEnable, createInfoMaxAnisotropy,
	createInfoCompareEnable, createInfoCompareOp,
	createInfoMinLod, createInfoMaxLod,
	createInfoBorderColor, createInfoUnnormalizedCoordinates

	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Data.Word
import Data.Int

import qualified Gpu.Vulkan.AllocationCallbacks.Core as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Core as Device

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
		[| #{poke VkSamplerCreateInfo, addressModeW} |]),
	("mipLodBias", ''#{type float},
		[| #{peek VkSamplerCreateInfo, mipLodBias} |],
		[| #{poke VkSamplerCreateInfo, mipLodBias} |]),
	("anisotropyEnable", ''#{type VkBool32},
		[| #{peek VkSamplerCreateInfo, anisotropyEnable} |],
		[| #{poke VkSamplerCreateInfo, anisotropyEnable} |]),
	("maxAnisotropy", ''#{type float},
		[| #{peek VkSamplerCreateInfo, maxAnisotropy} |],
		[| #{poke VkSamplerCreateInfo, maxAnisotropy} |]),
	("compareEnable", ''#{type VkBool32},
		[| #{peek VkSamplerCreateInfo, compareEnable} |],
		[| #{poke VkSamplerCreateInfo, compareEnable} |]),
	("compareOp", ''#{type VkCompareOp},
		[| #{peek VkSamplerCreateInfo, compareOp} |],
		[| #{poke VkSamplerCreateInfo, compareOp} |]),
	("minLod", ''#{type float},
		[| #{peek VkSamplerCreateInfo, minLod} |],
		[| #{poke VkSamplerCreateInfo, minLod} |]),
	("maxLod", ''#{type float},
		[| #{peek VkSamplerCreateInfo, maxLod} |],
		[| #{poke VkSamplerCreateInfo, maxLod} |]),
	("borderColor", ''#{type VkBorderColor},
		[| #{peek VkSamplerCreateInfo, borderColor} |],
		[| #{poke VkSamplerCreateInfo, borderColor} |]),
	("unnormalizedCoordinates", ''#{type VkBool32},
		[| #{peek VkSamplerCreateInfo, unnormalizedCoordinates} |],
		[| #{poke VkSamplerCreateInfo, unnormalizedCoordinates} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkCreateSampler" create ::
	Device.D -> Ptr CreateInfo -> Ptr AllocationCallbacks.A -> Ptr S ->
	IO #{type VkResult}

foreign import ccall "vkDestroySampler" destroy ::
	Device.D -> S -> Ptr AllocationCallbacks.A -> IO ()
