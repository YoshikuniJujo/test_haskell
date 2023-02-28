{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanSampleEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Sample.Enum" ["Data.Bits", "Data.Word"] [
	(	[("CountFlagsZero", Int 0)],
		(	"CountFlagBits", "VkSampleCountFlagBits",
			["Show", "Eq", "Storable", "Bits", "FiniteBits"] ) ) ]
	"type CountFlags = CountFlagBits"
