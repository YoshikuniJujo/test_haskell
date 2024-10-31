{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanQueueEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Queue.Enum" ["Data.Bits", "Data.Word"] [
	(	[("FlagsZero", Int 0)],
		(	"FlagBits", "VkQueueFlagBits",
			["Show", "Eq", "Storable", "Bits", "FiniteBits"] ) ) ]
	"type Flags = FlagBits"
