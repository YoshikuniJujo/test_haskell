{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanExtDebugUtilsMessageEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
	"Ext.DebugUtils.Enum" ["Data.Bits", "Data.Word"] [
		("MessageSeverityFlagBits", "VkDebugUtilsMessageSeverityFlagBitsEXT",
			["Show", "Eq", "Storable", "Bits"]),
		("MessageTypeFlagBits", "VkDebugUtilsMessageTypeFlagBitsEXT",
			["Show", "Eq", "Storable", "Bits"])
		]
	[nowdoc|
type MessageSeverityFlags = MessageSeverityFlagBits
type MessageTypeFlags = MessageTypeFlagBits|]
