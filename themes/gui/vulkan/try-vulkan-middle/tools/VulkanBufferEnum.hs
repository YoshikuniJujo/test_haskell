{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanBufferEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFileWithDefault vulkanCore "Buffer.Enum"
		["Data.Default", "Data.Bits", "Data.Word"] [
	(	Just "CreateFlagsZero", [("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkBufferCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	Just "UsageFlagsZero", [("UsageFlagsZero", Int 0)],
		(	"UsageFlagBits", "VkBufferUsageFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ) ]
	[nowdoc|
type CreateFlags = CreateFlagBits
type UsageFlags = UsageFlagBits|]
