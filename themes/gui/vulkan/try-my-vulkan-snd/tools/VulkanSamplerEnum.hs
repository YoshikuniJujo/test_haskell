{-# LANGUAGE QuasiQuotes #-}
{-# OPTiONS_GHC -Wall -fno-warn-tabs #-}

module VulkanSamplerEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Sampler.Enum" ["Data.Bits", "Data.Word"] [
	(	[("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkSamplerCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[],
		(	"MipmapMode", "VkSamplerMipmapMode",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[],
		(	"AddressMode", "VkSamplerAddressMode",
			["Show", "Eq", "Storable", "Bits"] ) )
	]
	[nowdoc|
type CreateFlags = CreateFlagBits|]
