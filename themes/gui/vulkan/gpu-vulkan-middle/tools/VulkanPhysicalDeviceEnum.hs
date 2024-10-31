{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanPhysicalDeviceEnum where

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
	"PhysicalDevice.Enum" ["Data.Word"]
	[("Type", "VkPhysicalDeviceType", ["Show", "Eq", "Storable"])] []
