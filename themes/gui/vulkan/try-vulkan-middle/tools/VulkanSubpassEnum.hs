{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanSubpassEnum where

import MakeEnum

make :: IO ()
make = createFile'' "/usr/include/vulkan/vulkan_core.h"
		"Subpass.Enum" ["Data.Bits", "Data.Word"] [
	(	[("DescriptionFlagsZero", Int 0)],
		(	"DescriptionFlagBits", "VkSubpassDescriptionFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[],
		(	"Contents", "VkSubpassContents",
			["Show", "Eq", "Storable"] ) )
	]
	"type DescriptionFlags = DescriptionFlagBits"
