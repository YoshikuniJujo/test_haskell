{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanMemoryEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Memory.Enum" ["Data.Bits", "Data.Word"] [
	(	[("PropertyFlagsZero", Int 0)],
		(	"PropertyFlagBits", "VkMemoryPropertyFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ) ]
	"type PropertyFlags = PropertyFlagBits"
