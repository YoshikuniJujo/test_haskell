{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanKhrSwapchainEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFile' "/usr/include/vulkan/vulkan_core.h"
		"Khr.Swapchain.Enum" ["Data.Bits", "Data.Word"] [
	("CreateFlagBits", "VkSwapchainCreateFlagBitsKHR",
		["Show", "Eq", "Storable", "Bits"]) ]
	[nowdoc|
type CreateFlags = CreateFlagBits|]
