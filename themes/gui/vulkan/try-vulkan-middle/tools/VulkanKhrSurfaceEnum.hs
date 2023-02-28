{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanKhrSurfaceEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
		"Khr.Surface.Enum" ["Data.Bits", "Data.Word"] [
	("TransformFlagBits", "VkSurfaceTransformFlagBitsKHR",
		["Show", "Eq", "Storable", "Bits"]) ]
	[nowdoc|
type TransformFlags = TransformFlagBits|]
