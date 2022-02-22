{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanExtDebugUtilsMessageEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
	"Ext.DebugUtils.Message.Enum" ["Data.Bits", "Data.Word"] [
		("SeverityFlagBits", "VkDebugUtilsMessageSeverityFlagBitsEXT",
			["Show", "Eq", "Storable", "Bits"]),
		("TypeFlagBits", "VkDebugUtilsMessageTypeFlagBitsEXT",
			["Show", "Eq", "Storable", "Bits"])
		]
	[nowdoc|
type SeverityFlags = SeverityFlagBits
type TypeFlags = TypeFlagBits|]
