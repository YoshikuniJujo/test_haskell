{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanBufferEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Buffer.Enum" ["Data.Bits", "Data.Word"] [
	(	[("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkBufferCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ) ]
	"type CreateFlags = CreateFlagBits"
