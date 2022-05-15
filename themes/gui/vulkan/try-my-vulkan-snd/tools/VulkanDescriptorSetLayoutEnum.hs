{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanDescriptorSetLayoutEnum where

import MakeEnum

make :: IO ()
make = createFile'' "/usr/include/vulkan/vulkan_core.h" "Descriptor.Set.Layout.Enum"
		["Data.Word", "Data.Bits"] [
	(	[("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkDescriptorSetLayoutCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) )
	]
	"type CreateFlags = CreateFlagBits"
