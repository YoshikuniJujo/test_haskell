{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanColorComponentEnum where

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
	"ColorComponent.Enum" ["Data.Bits", "Data.Word"]
	[("FlagBits", "VkColorComponentFlagBits",
		["Show", "Eq", "Storable", "Bits"])]
	"type Flags = FlagBits"
