{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanDescriptorEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Descriptor.Enum" ["Data.Bits", "Data.Word"] [
	(	[],
		("Type", "VkDescriptorType", ["Show", "Eq", "Storable"]) ),
	(	[],
		("BindingFlagBits", "VkDescriptorBindingFlagBits",
			["Show", "Eq", "Storable", "Bits"]) ) ]
	"type BindingFlags = BindingFlagBits"
