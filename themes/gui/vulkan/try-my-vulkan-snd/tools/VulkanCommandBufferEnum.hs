{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_gHC -Wall -fno-warn-tabs #-}

module VulkanCommandBufferEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "CommandBuffer.Enum" ["Data.Bits", "Data.Word"] [
	(	[],
		(	"Level", "VkCommandBufferLevel",
			["Show", "Eq", "Storable"] ) ),
	(	[("UsageFlagsZero", Int 0)],
		(	"UsageFlagBits", "VkCommandBufferUsageFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[("ResetFlagsZero", Int 0)],
		(	"ResetFlagBits", "VkCommandBufferResetFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) )
	] [nowdoc|
type UsageFlags = UsageFlagBits
type ResetFlags = ResetFlagBits|]
