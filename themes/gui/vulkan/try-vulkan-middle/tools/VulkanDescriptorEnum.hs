{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanDescriptorEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Descriptor.Enum" ["Data.Word"] [
	(	[],
		("Type", "VkDescriptorType", ["Show", "Eq", "Storable"]) ),
	(	[],
		("BindingFlagBits", "VkDescriptorBindingFlagBits",
			["Show", "Eq", "Storable"]) ) ]
	"type BindingFlags = BindingFlagBits"
