{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanDeviceQueueEnum where

import MakeEnum

make :: IO ()
make = createFileWithDefault vulkanCore "Device.Queue.Enum" ["Data.Bits", "Data.Word"]
	[(	Just "CreateFlagsZero", [("CreateFlagsZero", Int 0)],
		("CreateFlagBits", "VkDeviceQueueCreateFlagBits", ["Show", "Eq", "Storable", "Bits"]) )]
	"type CreateFlags = CreateFlagBits"
