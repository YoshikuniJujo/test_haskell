{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanKhrEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
		"Khr.Enum" ["Data.Bits", "Data.Word"] [
	("CompositeAlphaFlagBits", "VkCompositeAlphaFlagBitsKHR",
		["Show", "Eq", "Storable", "Bits"]),
	("ColorSpace", "VkColorSpaceKHR", ["Show", "Eq", "Storable"]),
	("PresentMode", "VkPresentModeKHR", ["Show", "Eq", "Storable"]) ]
	[nowdoc|
type CompositeAlphaFlags = CompositeAlphaFlagBits|]
