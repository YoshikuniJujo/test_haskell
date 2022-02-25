{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanSampleEnum where

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
	"Sample.Enum" ["Data.Bits", "Data.Word"]
	[("CountFlagBits", "VkSampleCountFlagBits",
		["Show", "Eq", "Storable", "Bits"])]
	"type CountFlags = CountFlagBits"
