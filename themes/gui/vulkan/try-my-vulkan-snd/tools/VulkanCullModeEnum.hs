{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanCullModeEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile'' "/usr/include/vulkan/vulkan_core.h"
		"CullMode.Enum" ["Data.Bits", "Data.Word"] [
	(	[("FlagsZero", Int 0)],
		(	"FlagBits", "VkCullModeFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) )
	]
	[nowdoc|
type Flags = FlagBits|]
