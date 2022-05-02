{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanBufferEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Buffer.Enum" ["Data.Bits", "Data.Word"] [
	(	[("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkBufferCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[("UsageFlagsZero", Int 0)],
		(	"UsageFlagBits", "VkBufferUsageFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ) ]
	[nowdoc|
type CreateFlags = CreateFlagBits
type UsageFlags = UsageFlagBits|]
