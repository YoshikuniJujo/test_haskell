{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanImageEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile'' "/usr/include/vulkan/vulkan_core.h"
		"Image.Enum" ["Data.Bits", "Data.Word"] [
	(	[("UsageFlagsZero", Int 0)],
		("UsageFlagBits", "VkImageUsageFlagBits",
			["Show", "Eq", "Storable", "Bits"])),
	(	[("AspectFlagsZero", Int 0)],
		("AspectFlagBits", "VkImageAspectFlagBits",
			["Show", "Eq", "Storable", "Bits"]))
	]
	[nowdoc|
type UsageFlags = UsageFlagBits
type AspectFlags = AspectFlagBits|]
