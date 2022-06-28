{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanDescriptorSetLayoutEnum where

import MakeEnum

make :: IO ()
make = createFileWithDefault "/usr/include/vulkan/vulkan_core.h" "DescriptorSetLayout.Enum"
		["Data.Word", "Data.Bits"] [
	(	Just "CreateFlagsZero", [("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkDescriptorSetLayoutCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) )
	]
	"type CreateFlags = CreateFlagBits"
