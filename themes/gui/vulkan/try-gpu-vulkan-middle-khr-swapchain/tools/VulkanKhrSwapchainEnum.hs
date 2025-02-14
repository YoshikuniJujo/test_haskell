{-# LANGUAGE QuasiQuotes #-}

module VulkanKhrSwapchainEnum where

import Text.Nowdoc

import MakeEnum

make :: IO ()
make = createFileWithDefault vulkanCore "Khr.Swapchain.Enum"
		["Data.Bits", "Data.Word"] [
	(	Just "CreateFlagsZero", [("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkSwapchainCreateFlagBitsKHR",
			["Show", "Eq", "Storable", "Bits"] ) )
	]
	[nowdoc|
type CreateFlags = CreateFlagBits|]
