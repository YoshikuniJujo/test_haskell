{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanDeviceQueueEnum where

import MakeEnum

make :: IO ()
make = createFileWithDefault vulkanCore "Device.Enum" ["Data.Bits", "Data.Word"]
	[(	Just "QueueCreateFlagsZero", [("QueueCreateFlagsZero", Int 0)],
		("QueueCreateFlagBits", "VkDeviceQueueCreateFlagBits", ["Show", "Eq", "Storable", "Bits"]) )]
	"type QueueCreateFlags = QueueCreateFlagBits"
