{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanKhrSurfaceEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
		"Khr.Surface.Enum" ["Data.Bits", "Data.Word"] [
	("TransformFlagBits", "VkSurfaceTransformFlagBitsKHR",
		["Show", "Eq", "Storable", "Bits"]),
	("CompositeAlphaFlagBits", "VkCompositeAlphaFlagBitsKHR",
		["Show", "Eq", "Storable", "Bits"]),
	("ColorSpace", "VkColorSpaceKHR", ["Show", "Eq", "Storable"]),
	("PresentMode", "VkPresentModeKHR", ["Show", "Eq", "Storable"]) ]
	[nowdoc|

type TransformFlags = TransformFlagBits
type CompositeAlphaFlags = CompositeAlphaFlagBits|]
