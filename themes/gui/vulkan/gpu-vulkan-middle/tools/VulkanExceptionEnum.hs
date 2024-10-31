{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanExceptionEnum where

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h" "Exception.Enum"
	["Data.Int"]
	[("Result", "VkResult", ["Show", "Read", "Eq", "Enum", "Storable"])]
	"type PtrResult = Result"
