{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanFenceEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Fence.Enum" ["Data.Bits", "Data.Word"] [
	(	[("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkFenceCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ) ]
	"type CreateFlags = CreateFlagBits"
