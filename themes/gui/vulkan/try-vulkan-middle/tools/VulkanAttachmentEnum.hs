{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanAttachmentEnum where

import MakeEnum

make :: IO ()
make = createFile'' "/usr/include/vulkan/vulkan_core.h"
		"Attachment.Enum" ["Data.Bits", "Data.Word"] [
	(	[("DescriptionFlagsZero", Int 0)],
		(	"DescriptionFlagBits", "VkAttachmentDescriptionFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ),
	(	[],
		(	"LoadOp", "VkAttachmentLoadOp",
			["Show", "Eq", "Storable"] ) ),
	(	[],
		(	"StoreOp", "VkAttachmentStoreOp",
			["Show", "Eq", "Storable"] ) )
	]
	"type DescriptionFlags = DescriptionFlagBits"
