{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_gHC -Wall -fno-warn-tabs #-}

module VulkanCommandBufferEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFileWithDefault vulkanCore "CommandBuffer.Enum"
		["Data.Default", "Data.Bits", "Data.Word"] [
	(	Nothing, [],
		(	"Level", "VkCommandBufferLevel",
			["Show", "Eq", "Storable"] ) ),
	(	Just "UsageFlagsZero", [("UsageFlagsZero", Int 0)],
		(	"UsageFlagBits", "VkCommandBufferUsageFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	Just "ResetFlagsZero", [("ResetFlagsZero", Int 0)],
		(	"ResetFlagBits", "VkCommandBufferResetFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) )
	] [nowdoc|
type UsageFlags = UsageFlagBits
type ResetFlags = ResetFlagBits|]
