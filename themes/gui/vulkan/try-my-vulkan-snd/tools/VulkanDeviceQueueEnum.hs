{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanDeviceQueueEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Device.Queue.Enum" ["Data.Bits", "Data.Word"]
	[(	[("CreateFlagsZero", Int 0)],
		("CreateFlagBits", "VkDeviceQueueCreateFlagBits", ["Show", "Eq", "Storable", "Bits"]) )]
	"type CreateFlags = CreateFlagBits"
