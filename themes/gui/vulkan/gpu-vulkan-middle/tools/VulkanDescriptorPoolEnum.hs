{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanDescriptorPoolEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "DescriptorPool.Enum"
		["Data.Bits", "Data.Word"] [
	(	[("CreateFlagsZero", Int 0)],	
		("CreateFlagBits", "VkDescriptorPoolCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"]) ) ]
	"type CreateFlags = CreateFlagBits"
