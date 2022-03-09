{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanImageViewEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile'' "/usr/include/vulkan/vulkan_core.h"
		"ImageView.Enum" ["Data.Bits", "Data.Word"] [
	(	[("CreateFlagsZero", Int 0)],
		("CreateFlagBits", "VkImageViewCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"]) ),
	(	[],
		("Type", "VkImageViewType",
			["Show", "Eq", "Storable"]) )
	]
	[nowdoc|
type CreateFlags = CreateFlagBits|]
