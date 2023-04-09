{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanCommandPoolEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "CommandPool.Enum" ["Data.Bits", "Data.Word"] [
	(	[("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkCommandPoolCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[],
		(	"ResetFlagBits", "VkCommandPoolResetFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ) ]
	"type CreateFlags = CreateFlagBits\ntype ResetFlags = ResetFlagBits"
