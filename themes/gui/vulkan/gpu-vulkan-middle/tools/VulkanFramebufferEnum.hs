{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VulkanFramebufferEnum where

import MakeEnum

make :: IO ()
make = createFile'' vulkanCore "Framebuffer.Enum" ["Data.Bits", "Data.Word"] [
	(	[("CreateFlagsZero", Int 0)],
		(	"CreateFlagBits", "VkFramebufferCreateFlagBits",
			["Show", "Eq", "Storable", "Bits"] ) ) ]
	"type CreateFlags = CreateFlagBits"
