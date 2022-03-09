{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanComponentEnum where

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
	"Component.Enum" ["Data.Word"]
	[("Swizzle", "VkComponentSwizzle", ["Show", "Eq", "Storable"])] []
