{-# OPTIONS_gHC -Wall -fno-warn-tabs #-}

module VulkanCommandBufferEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "CommandBuffer.Enum" ["Data.Bits", "Data.Word"] [
	(	[],
		(	"Level", "VkCommandBufferLevel",
			["Show", "Eq", "Storable"] ) ),
	(	[("UsageFlagsZero", Int 0)],
		(	"UsageFlagBits", "VkCommandBufferUsageFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) )
	]
	"type UsageFlags = UsageFlagBits"
