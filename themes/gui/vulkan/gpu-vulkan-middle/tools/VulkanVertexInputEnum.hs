{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanVertexInputEnum where

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
	"VertexInput.Enum" ["Data.Word"]
	[("Rate", "VkVertexInputRate", ["Show", "Eq", "Storable"])] ""
